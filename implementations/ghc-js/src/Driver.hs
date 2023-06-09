{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Driver where

import Hardware.MOS6502.Emu
import Data.Coerce

import Data.Array.IO
import qualified Data.Array as Arr
import qualified Data.Array.MArray as Arr
import qualified Data.ByteString as BS

import Data.Word
import Control.Monad (zipWithM_)
import Control.Monad.Reader
import System.FilePath
import Text.Printf

newtype Memory m a = Memory{ runMemory :: ReaderT (IOUArray Addr Byte) m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadMachine (Memory m) where
    readMem addr = Memory $ do
        mem <- ask
        liftIO $ Arr.readArray mem addr

    writeMem addr v = Memory $ do
        mem <- ask
        liftIO $ Arr.writeArray mem addr v

run :: BS.ByteString -> IO Int
run bs = do
    mem <- Arr.newArray (minBound, maxBound) 0x00
    zipWithM_ (Arr.writeArray mem) [0x0000..] (BS.unpack bs)

    cpu <- new 0x438b
    let runCPU = flip runReaderT mem . runMemory . flip runReaderT cpu

    let loop cnt = getReg pc >>= \case
            0x640b -> pure cnt
            pc -> step >> loop (cnt + 1)
    runCPU $ loop 0
