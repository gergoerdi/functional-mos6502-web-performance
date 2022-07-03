{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Driver where

import Hardware.MOS6502.Emu
import Data.Coerce

import GHCJS.Marshal
import GHCJS.Foreign.Callback
import Data.JSString (JSString, unpack, pack)
import GHCJS.Types
import JavaScript.TypedArray
import JavaScript.TypedArray.ArrayBuffer
import JavaScript.Object

import Data.Word
import Control.Monad.Reader
import System.FilePath
import Text.Printf


newtype Memory m a = Memory{ runMemory :: ReaderT Uint8Array m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadMachine (Memory m) where
    readMem addr = Memory $ do
        mem <- ask
        liftIO $ index (fromIntegral addr) mem

    writeMem addr v = Memory $ do
        mem <- ask
        liftIO $ setIndex (fromIntegral addr) (fromIntegral v) mem

run :: MutableArrayBuffer -> IO Int
run buf = do
    arr <- fromArrayBuffer buf 0 Nothing

    cpu <- new 0x438b
    let runCPU = flip runReaderT arr . runMemory . flip runReaderT cpu

    let loop cnt = getReg pc >>= \case
            0x640b -> pure cnt
            pc -> step >> loop (cnt + 1)
    runCPU $ loop 0

-- Exporting Haskell code to JS is annoying, see
-- https://stackoverflow.com/q/29967135/946226

foreign import javascript unsafe "ghcjs_callback_ = $1"
    set_callback :: Callback a -> IO ()

returnViaArgument :: (FromJSVal a, ToJSVal b) => (a -> IO b) -> JSVal -> JSVal -> IO ()
returnViaArgument f arg retJSVal = do
    retObj <- fromJSValUnchecked retJSVal
    r <- f arg
    rv <- toJSVal r
    setProp "ret" rv retObj

main = do
    callback <- syncCallback2 ThrowWouldBlock (returnViaArgument run)
    set_callback callback

