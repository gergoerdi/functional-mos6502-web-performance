{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Driver where

import Hardware.MOS6502.Emu
import Asterius.Types
import Data.Coerce

import Data.Word
import Control.Monad.Reader
import System.FilePath
import Text.Printf

newtype JSArrayBuffer = JSArrayBuffer JSVal

foreign import javascript "new Uint8Array($1)" js_toUint8Array :: JSArrayBuffer -> IO JSUint8Array
foreign import javascript "$1[$2]" js_readArr :: JSUint8Array -> Int -> IO Int
foreign import javascript "$1[$2] = $3" js_writeArr :: JSUint8Array -> Int -> Int -> IO ()

newtype Memory m a = Memory{ runMemory :: ReaderT JSUint8Array m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadMachine (Memory m) where
    readMem addr = Memory $ do
        mem <- ask
        liftIO $ fromIntegral <$> js_readArr mem (fromIntegral addr)

    writeMem addr v = Memory $ do
        mem <- ask
        liftIO $ js_writeArr mem (fromIntegral addr) (fromIntegral v)

{-# INLINEABLE callJSFunction #-}
callJSFunction :: JSVal -> [JSVal] -> IO JSVal
callJSFunction f args = js_function_apply f js_object_empty (toJSArray args)

foreign import javascript "{}" js_object_empty :: JSVal
foreign import javascript "$1.apply($2, $3)" js_function_apply :: JSVal -> JSVal -> JSArray -> IO JSVal

newtype JSFileLoader = JSFileLoader JSVal

loadFile :: JSFileLoader -> FilePath -> IO JSArrayBuffer
loadFile (JSFileLoader f) fp = coerce $ callJSFunction f [coerce $ toJSString fp]

foreign export javascript "run" run :: JSFileLoader -> IO Int
run :: JSFileLoader -> IO Int
run fileLoader = do
    mem <- js_toUint8Array =<< readFile "data/program.dat"

    cpu <- new 0x438b
    let runCPU = flip runReaderT mem . runMemory . flip runReaderT cpu

    let loop cnt = getReg pc >>= \case
            0x640b -> pure cnt
            pc -> step >> loop (cnt + 1)
    runCPU $ loop 0
  where
    readFile = loadFile fileLoader
