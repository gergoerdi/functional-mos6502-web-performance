module Main (run) where

import Hardware.MOS6502.Emu

import Prelude
import Data.Integral (fromIntegral)
import Data.Maybe (Maybe(..), fromJust)
import Data.UInt (fromInt, toInt)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Reader.Class
import Uncurried.ReaderT

import Control.Monad.Rec.Class

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8, DataView, ByteOffset)
import Data.ArrayBuffer.DataView as Arr

newtype Memory m a = Memory (ReaderT (DataView) m a)

instance Functor m => Functor (Memory m) where
    map f = Memory <<< map f <<< runMemory
    
instance Apply m => Apply (Memory m) where
    apply ff fx = Memory $ apply (runMemory ff) (runMemory fx)

instance Applicative m => Applicative (Memory m) where
    pure = Memory <<< pure

instance Bind m => Bind (Memory m) where
    bind m k = Memory $ bind (runMemory m) (runMemory <<< k)

instance Monad m => Monad (Memory m) 

instance MonadRec m => MonadRec (Memory m) where
    tailRecM step start = Memory $ tailRecM (runMemory <<< step) start

instance MonadEffect m => MonadEffect (Memory m) where
    liftEffect act = Memory $ liftEffect act

instance MonadEffect m => MonadMachine (Memory m) where
    readMem addr = unsafePartial do
        mem <- Memory ask
        liftEffect $ fromIntegral <<< toInt <<< fromJust <$> Arr.getUint8 mem (fromIntegral addr)

    writeMem addr v = do
        mem <- Memory ask
        void $ liftEffect $ Arr.setUint8 mem (fromIntegral addr) (fromInt $ fromIntegral v)

runMemory :: forall m a. Memory m a -> ReaderT (DataView) m a
runMemory (Memory act) = act

run :: ArrayBuffer -> Effect Int
run buf = do
    let mem = Arr.whole buf

    cpu <- new $ fromIntegral 0x438b
    let runCPU :: forall a. ReaderT CPU (Memory Effect) a -> Effect a
        runCPU = runReaderT mem <<< runMemory <<< runReaderT cpu

    unsafePartial $ runCPU $ flip tailRecM 0 $ \cnt -> do
        getReg _.pc >>= \pc -> case fromIntegral pc of
            0x640b -> pure $ Done cnt
            _ -> do
                step
                pure $ Loop $ cnt + 1
