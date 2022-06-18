module Data.Bits.Fast

-- Data.Bits uses `Fin n`, which is represented as a BigInt
-- We use the raw primitive bitops here. Just comment this out
-- and import Data.Bits again to measure the impact this has.

%default total

infixl 8 `shiftL`, `shiftR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

namespace Bits8
  public export %inline
  (.|.) : Bits8 -> Bits8 -> Bits8
  (.|.) = prim__or_Bits8

  public export %inline
  (.&.) : Bits8 -> Bits8 -> Bits8
  (.&.) = prim__and_Bits8

  public export %inline
  xor : Bits8 -> Bits8 -> Bits8
  xor = prim__xor_Bits8

  public export %inline
  shiftL : Bits8 -> Bits8 -> Bits8
  shiftL = prim__shl_Bits8

  public export %inline
  shiftR : Bits8 -> Bits8 -> Bits8
  shiftR = prim__shr_Bits8

  public export %inline
  bit : Bits8 -> Bits8
  bit = shiftL 1

  public export %inline
  setBit : Bits8 -> Bits8 -> Bits8
  setBit x i = x .|. bit i

  public export %inline
  testBit : Bits8 -> Bits8 -> Bool
  testBit x i = (x .&. bit i) /= 0

  public export %inline
  clearBit : Bits8 -> Bits8 -> Bits8
  clearBit x i = x `xor` (bit i .&. x)

namespace Bits16
  public export %inline
  (.|.) : Bits16 -> Bits16 -> Bits16
  (.|.) = prim__or_Bits16

  public export %inline
  (.&.) : Bits16 -> Bits16 -> Bits16
  (.&.) = prim__and_Bits16

  public export %inline
  xor : Bits16 -> Bits16 -> Bits16
  xor = prim__xor_Bits16

  public export %inline
  shiftL : Bits16 -> Bits16 -> Bits16
  shiftL = prim__shl_Bits16

  public export %inline
  shiftR : Bits16 -> Bits16 -> Bits16
  shiftR = prim__shr_Bits16

  public export %inline
  bit : Bits16 -> Bits16
  bit = shiftL 1

  public export %inline
  setBit : Bits16 -> Bits16 -> Bits16
  setBit x i = x .|. bit i

  public export %inline
  testBit : Bits16 -> Bits16 -> Bool
  testBit x i = (x .&. bit i) /= 0

  public export %inline
  clearBit : Bits16 -> Bits16 -> Bits16
  clearBit x i = x `xor` (bit i .&. x)
