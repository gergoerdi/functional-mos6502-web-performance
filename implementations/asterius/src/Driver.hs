{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Driver where

import Hardware.MOS6502.Emu
import Asterius.Types
import Data.Coerce

import Asterius.ByteString
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as V
import qualified Data.ByteString as BS

import Data.Word
import Control.Monad.Reader
import System.FilePath
import Text.Printf

newtype JSArrayBuffer = JSArrayBuffer JSVal

foreign import javascript "new Uint8Array($1)" js_toUint8Array :: JSArrayBuffer -> IO JSUint8Array

newtype Memory m a = Memory{ runMemory :: ReaderT (V.IOVector Byte) m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadMachine (Memory m) where
    readMem addr = Memory $ do
        mem <- ask
        liftIO $ V.read mem (fromIntegral addr)

    writeMem addr v = Memory $ do
        mem <- ask
        liftIO $ V.write mem (fromIntegral addr) (fromIntegral v)

foreign export javascript "run" run :: JSArrayBuffer -> IO Int
run :: JSArrayBuffer -> IO Int
run buf = do
    arr <- js_toUint8Array buf
    let bs = byteStringFromJSUint8Array arr
    mem <- V.new 0x10000
    zipWithM_ (V.write mem) [0x0000..] (BS.unpack bs)

    cpu <- new 0x438b
    let runCPU = flip runReaderT mem . runMemory . flip runReaderT cpu

    let loop cnt = getReg pc >>= \case
            0x640b -> pure cnt
            pc -> step >> loop (cnt + 1)
    runCPU $ loop 0
