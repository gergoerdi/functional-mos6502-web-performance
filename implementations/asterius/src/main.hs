{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Main where

import Hardware.MOS6502.Emu

import Data.Word
import Control.Monad.Reader
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as V
import qualified Data.ByteString as BS
import System.FilePath
import Text.Printf

newtype Memory m a = Memory{ runMemory :: ReaderT (V.IOVector Byte) m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadMachine (Memory m) where
    readMem addr = Memory $ do
        mem <- ask
        liftIO $ V.read mem (fromIntegral addr)

    writeMem addr v = Memory $ do
        mem <- ask
        liftIO $ V.write mem (fromIntegral addr) v

main :: IO ()
main = do
    img <- BS.readFile "data/program.dat"
    mem <- V.new 0x10000
    zipWithM_ (V.write mem) [0x0000..] (BS.unpack img)

    cpu <- new 0x438b
    let runCPU = flip runReaderT mem . runMemory . flip runReaderT cpu

    let loop cnt = getReg pc >>= \case
            0x640b -> pure cnt
            pc -> step >> loop (cnt + 1)
    cnt <- runCPU $ loop 0
    print cnt
