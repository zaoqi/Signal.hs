--Copyright (C) 2017  Zaoqi

--This program is free software: you can redistribute it and/or modify
--it under the terms of the GNU Affero General Public License as published
--by the Free Software Foundation, either version 3 of the License, or
--(at your option) any later version.

--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU Affero General Public License for more details.

--You should have received a copy of the GNU Affero General Public License
--along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Control.Concurrent.Signal (
    newSignal,
    newStreamSignal,
    runSignal,
    runStreamSignal,
    scanp,
    sampleOn,
    slift,
    sliftinit,
    isStreamSignal,
    noSampleOn
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.IORef
import Control.Exception

catch_ :: IO () -> IO ()
catch_ x =
    x
    `catch` \(SomeException _) -> return ()

data Signal a = Signal ((a -> IO ()) -> IO ()) | -- 有新信号时调用(a -> IO ())，不能同时多次调用，返回的IO ()用来注册
                Stream (IO (IO a)) --返回的IO a调用时返回下一个值

newSignal :: ((a -> IO ()) -> IO ()) -> Signal a
newSignal s = Signal $ \f -> s $ catch_ . f

newStreamSignal :: IO (IO a) -> Signal a
newStreamSignal = Stream

stream2Signal (Stream x) = newSignal $ \f -> do
    s <- x
    forkIO $ forever $ do
        i <- s
        f i
    return ()

runSignal :: Signal a -> (a -> IO ()) -> IO ()
runSignal (Signal x) = x

runStreamSignal :: Signal a -> (a -> IO ()) -> IO ()
runStreamSignal x = runSignal . stream2Signal $ x

instance Functor Signal where
    fmap f (Signal s) = Signal $ \n -> s $ n . f
    fmap f (Stream s) = Stream $ fmap (fmap f) s
    --fmap f (Stream s) = Stream $ do
        --g <- s
        --return $ do
            --x <- g
            --return $ f x

splus (Signal a) (Signal b) =
    let
        call ra rb f r i = do
            atomicWriteIORef r (Just i)
            ia <- readIORef ra
            ib <- readIORef rb
            case (,) <$> ia <*> ib of
                Just x -> f x
                Nothing -> return ()
    in Signal $ \f -> do
        ra <- newIORef Nothing
        rb <- newIORef Nothing
        b $ call ra rb f rb
        a $ call ra rb f ra
splus (Stream a) (Stream b) = Stream $ do
    fa <- a
    fb <- b
    return $ (,) <$> fa <*> fb
splus (Signal a) (Stream b) = Signal $ \f -> do
    fb <- b
    a $ \ia -> do
        ib <- fb
        f (ia, ib)
splus (Stream a) (Signal b) = Signal $ \f -> do
    fa <- a
    b $ \ib -> do
        ia <- fa
        f (ia, ib)

instance Applicative Signal where
    pure = Stream . return . return
    x <*> y = fmap (\(f, x) -> f x) $ splus x y

scanp :: (b -> a -> b) -> b -> Signal a -> Signal b
scanp f x (Signal s) = Signal $ \n -> do
    r <- newIORef x
    s $ \i -> do
        p <- readIORef r
        let ns = f p i
        writeIORef r ns
        n ns
scanp f x (Stream s) = Stream $ do
    fi <- s
    r <- newMVar x
    return $ do
        i <- fi
        uninterruptibleMask $ \restore -> do
            p <- takeMVar r
            let ns = f p i
            onException (restore $ do
                    putMVar r ns
                    return ns) (putMVar r p)

sampleOn :: Signal b -> Signal a -> Signal a
sampleOn (Stream _) x = x
sampleOn (Signal c) (Stream v) = Signal $ \n -> do
    fv <- v
    c $ \_ -> do
        i <- fv
        n i
sampleOn (Signal c) (Signal v) = Signal $ \n -> do
    r <- newIORef Nothing
    v $ \i -> atomicWriteIORef r (Just i)
    c $ \_ -> do
        i <- readIORef r
        case i of Just x -> n x
                  Nothing -> return ()

slift :: Signal (IO a) -> Signal a
slift (Signal s) = Signal $ \n -> s $ \f -> do
    x <- f
    n x
slift (Stream s) = Stream $ fmap join s

sliftinit :: IO a -> Signal a
sliftinit f = Stream $ do
    x <- f
    return . return $ x

isStreamSignal :: Signal a -> Bool
isStreamSignal (Stream _) = True
isStreamSignal _ = False

noSampleOn :: Signal a -> Signal a
noSampleOn (Signal f) = Stream $ do
    r <- newIORef undefined
    f $ atomicWriteIORef r
    return $ readIORef r