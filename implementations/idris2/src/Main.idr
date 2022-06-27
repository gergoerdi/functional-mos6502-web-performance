module Main

import Hardware.MOS6502.Emu

import Data.Array.Fast

import JS.Buffer
import JS.Array

0 Memory : Type
Memory = Array Byte

toMachine : Memory -> Machine
toMachine mem = MkMachine
  { readMem_  = \addr => readArray mem (cast addr)
  , writeMem_ = \addr => writeArray mem (cast addr)
  }

untilIO : acc -> (acc -> IO (Either acc a)) -> IO a
untilIO acc0 step = fromPrim $ go acc0
  where
    go : acc -> PrimIO a
    go acc w =
      let MkIORes (Left acc') w' = toPrim (step acc) w
            | MkIORes (Right res) w' => MkIORes res w'
      in go acc' w'

single : Machine => CPU => IO (Maybe ())
single = do
  getReg pc >>= \pc' => do
    case pc' of
      0x640b => pure $ Just ()
      _ => do
        step
        pure Nothing

%nomangle
public export
idris2_run : ArrayBuffer -> IO Nat
idris2_run buf = do
  mem <- arrayDataFrom . cast {to = UInt8Array} $ buf

  cpu <- new 0x438b
  let m = toMachine mem

  untilIO 0 $ \cnt => do
    Nothing <- single
      | Just _ => pure $ Right cnt
    pure $ Left $ cnt + 1

main : IO ()
main = pure ()
