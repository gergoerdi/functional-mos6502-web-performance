module Main

import Hardware.MOS6502.Emu

import Data.Maybe
import Data.String

import JS.Buffer
import JS.Array
import JS.Util

-- reads a value at the given index from a byte array
-- I reimplemented this here, because the version from
-- the dom library returns a Maybe (something we don't need
-- here), and has a call to `<$>`, which will conjure a
-- Monad IO record (something I should fix).
%foreign "javascript:lambda:(arr,n,w) => arr[n]"
prim__readArr : Array Byte -> Bits32 -> PrimIO Byte

-- writes a value to a mutable array
%foreign "javascript:lambda:(arr,n,v,w) => { arr[n] = v }"
prim__writeArr : Array Byte -> Bits32 -> Byte -> PrimIO ()

0 Memory : Type
Memory = Array Byte

readMemory : Memory -> Addr -> IO Byte
readMemory mem addr = fromPrim $ prim__readArr mem (cast addr)

toMachine : Memory -> Machine
toMachine mem = MkMachine
  { readMem_  = \addr => readMemory mem addr
  , writeMem_ = \addr => writeIO mem (cast addr)
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
