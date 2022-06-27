module Main (run) where

import Hardware.MOS6502.Emu

import Prelude
import Data.Integral (fromIntegral)
import Data.Maybe (Maybe(..), fromJust)
import Data.UInt (UInt, fromInt, toInt)
import Partial.Unsafe (unsafePartial)

import Control.Monad.Reader.Class
import Uncurried.ReaderT

import Control.Monad.Rec.Class

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8)
import Data.ArrayBuffer.Typed as Arr

newtype Memory m a = Memory (ReaderT (ArrayView Uint8) m a)

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

foreign import writeArray :: ArrayView Uint8 -> Arr.Index -> UInt -> Effect Unit

instance MonadEffect m => MonadMachine (Memory m) where
    readMem addr = unsafePartial do
        mem <- Memory ask
        liftEffect $ fromIntegral <<< toInt <$> Arr.unsafeAt mem (fromIntegral addr)

    writeMem addr v = do
        mem <- Memory ask
        liftEffect $ writeArray mem (fromIntegral addr) (fromInt $ fromIntegral v)

runMemory :: forall m a. Memory m a -> ReaderT (ArrayView Uint8) m a
runMemory (Memory act) = act

run :: ArrayBuffer -> Effect Int
run buf = do
    mem <- Arr.whole buf

    cpu <- new $ fromIntegral 0x438b
    let runCPU :: forall a. ReaderT CPU (Memory Effect) a -> Effect a
        runCPU = runReaderT mem <<< runMemory <<< runReaderT cpu

    unsafePartial $ runCPU $ flip tailRecM 0 $ \cnt -> do
        getReg _.pc >>= \pc -> case fromIntegral pc of
            0x640b -> pure $ Done cnt
            _ -> do
                step
                pure $ Loop $ cnt + 1
