{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Driver where

import Hardware.MOS6502.Emu
import Data.Coerce
import Unsafe.Coerce

import GHCJS.Marshal
import GHCJS.Foreign.Callback
import Data.JSString (JSString, unpack, pack)
import GHCJS.Types
import GHCJS.Buffer.Types
import JavaScript.TypedArray
import JavaScript.TypedArray.ArrayBuffer
import JavaScript.Object

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Data.Maybe

import Data.Word
import Control.Monad.Reader
import System.FilePath
import Text.Printf


-- IOUInt8Array is not exported by ghcjs-base.
-- But it seems to be the type that has the TypedArray instance?
-- TH to the rescue!
type IOUInt8Array = $(do
  TyConI (TySynD _ [] (someUInt8Array `AppT` immutable)) <- reify ''Uint8Array
  TyConI (TySynD _ [] (_ `AppT` mutable)) <- reify ''MutableBuffer
  pure $ someUInt8Array `AppT` mutable
 )

newtype Memory m a = Memory{ runMemory :: ReaderT IOUInt8Array m a }
    deriving newtype (Functor, Applicative, Monad, MonadIO)

instance (MonadIO m) => MonadMachine (Memory m) where
    readMem addr = Memory $ do
        mem <- ask
        liftIO $ index (fromIntegral addr) mem

    writeMem addr v = Memory $ do
        mem <- ask
        liftIO $ setIndex (fromIntegral addr) (fromIntegral v) mem

foreign import javascript unsafe "new Uint8Array($1)"
    uint8Array_from_MutableArrayBuffer :: MutableArrayBuffer -> IO IOUInt8Array

run :: MutableArrayBuffer -> IO Int
run buf = do
    -- arr <- fromArrayBuffer buf 0 Nothing
    arr <- uint8Array_from_MutableArrayBuffer buf

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

returnViaArgument :: ToJSVal b => (a -> IO b) -> JSVal -> JSVal -> IO ()
returnViaArgument f argJSVal retJSVal = do
    let retObj = unsafeCoerce retJSVal
    let arg = unsafeCoerce argJSVal
    r <- f arg
    rv <- toJSVal r
    setProp "ret" rv retObj

main = do
    callback <- syncCallback2 ThrowWouldBlock (returnViaArgument run)
    set_callback callback

