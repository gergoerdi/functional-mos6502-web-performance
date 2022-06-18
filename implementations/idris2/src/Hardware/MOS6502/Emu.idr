module Hardware.MOS6502.Emu

import Data.Bits.Fast
import Control.Monad.IO.Fast
import Data.IORef
import Data.String

%default total

-- TODO: not actually hexadecimal...
hex : forall a. Show a => Nat -> a -> String
hex n = padLeft n '0' . show

public export
Byte : Type
Byte = Bits8

public export
Addr : Type
Addr = Bits16

public export
record Machine where
  constructor MkMachine
  readMem_  : Addr -> IO Byte
  writeMem_ : Addr -> Byte -> IO ()

export %inline
readMem : Machine => Addr -> IO Byte
readMem = readMem_ %search

export %inline
writeMem : Machine => Addr -> Byte -> IO ()
writeMem = writeMem_ %search

public export
record CPU where
  constructor MkCPU
  regA, regX, regY, status, sp : IORef Byte
  pc : IORef Addr

public export
new : Addr -> IO CPU
new pc0 = pure $ MkCPU
  { regA   = !(newIORef 0x00)
  , regX   = !(newIORef 0x00)
  , regY   = !(newIORef 0x00)
  , status = !(newIORef 0x00)
  , sp     = !(newIORef 0xff)
  , pc     = !(newIORef pc0)
  }

fetch : Machine => (cpu : CPU) => IO Byte
fetch = do
  addr <- readIORef cpu.pc
  writeIORef cpu.pc (addr + 1)
  readMem addr

public export
toAddr : Byte -> Byte -> Addr
toAddr lo hi = (cast hi) `shiftL` 8 .|. cast lo

fetchAddr : Machine => CPU => IO Addr
fetchAddr = toAddr <$> fetch <*> fetch

readMemAddr : Machine => Addr -> IO Addr
readMemAddr addr = toAddr <$> readMem addr <*> readMem (addr + 1)

0 Reg8 : Type
Reg8 = CPU -> IORef Byte

public export
getReg : (cpu : CPU) => (CPU -> IORef a) -> IO a
getReg reg = readIORef (reg cpu)

public export
setReg : (cpu : CPU) => (CPU -> IORef a) -> a -> IO ()
setReg reg v = writeIORef (reg cpu) v

modifyReg : CPU => (CPU -> IORef a) -> (a -> a) -> IO a
modifyReg reg f = do
  v <- getReg reg
  setReg reg $ f v
  pure v

push : Machine => CPU => Byte -> IO ()
push v = do
    ptr <- modifyReg sp (`subtract` 1)
    writeMem (0x0100 + cast ptr) v

pushAddr : Machine => CPU => Addr -> IO ()
pushAddr addr = push hi >> push lo
  where
    hi, lo : Byte
    hi = cast $ addr `shiftR` 8
    lo = cast addr

pop : Machine => CPU => IO Byte
pop = do
  ptr <- modifyReg sp (+ 1)
  readMem (0x0100 + cast (ptr + 1))

popAddr : Machine => CPU => IO Addr
popAddr = toAddr <$> pop <*> pop

0 Flag : Type
Flag = Byte

getFlag : CPU => Flag -> IO Bool
getFlag flag = do
  flags <- getReg status
  pure $ flags `testBit` flag

setFlag : CPU => Flag -> Bool -> IO ()
setFlag flag b = ignore $ modifyReg status $ flip (if b then setBit else clearBit) flag

-- Unfortunately, Idris does not inline these automatically,
-- so we add an `%inline` pragma here.
%inline
carry, zero, interruptEnable, decimal, overflow, negative: Flag
carry = 0
zero = 1
interruptEnable = 2
decimal = 3
overflow = 6
negative = 7

public export
rts : Machine => CPU => IO ()
rts = do
    addr <- popAddr
    setReg pc (addr + 1)

public export
step : Machine => CPU => IO ()
step = fetch >>= \op => case op of -- http://www.6502.org/tutorials/6502opcodes.html
  0x69 => imm adc
  0x65 => byVal zp adc
  0x75 => byVal zpX adc
  0x6d => byVal abs adc
  0x7d => byVal absX adc
  0x79 => byVal absY adc
  0x61 => byVal xInd adc
  0x71 => byVal indY adc

  0x29 => imm and
  0x25 => byVal zp and
  0x35 => byVal zpX and
  0x2d => byVal abs and
  0x3d => byVal absX and
  0x39 => byVal absY and
  0x21 => byVal xInd and
  0x31 => byVal indY and

  0x0a => implied regA asl
  0x06 => inplace zp asl
  0x16 => inplace zpX asl
  0x0e => inplace abs asl
  0x1e => inplace absX asl

  0x24 => byVal zp bit
  0x2c => byVal abs bit

  0x10 => br negative False
  0x30 => br negative True
  0x50 => br overflow False
  0x70 => br overflow True
  0x90 => br carry False
  0xb0 => br carry True
  0xd0 => br zero False
  0xf0 => br zero True

--   -- 0x00 => brk -- TODO

  0xc9 => imm $ cmp regA
  0xc5 => byVal zp $ cmp regA
  0xd5 => byVal zpX $ cmp regA
  0xcd => byVal abs $ cmp regA
  0xdd => byVal absX $ cmp regA
  0xd9 => byVal absY $ cmp regA
  0xc1 => byVal xInd $ cmp regA
  0xd1 => byVal indY $ cmp regA

  0xe0 => imm $ cmp regX
  0xe4 => byVal zp $ cmp regX
  0xec => byVal abs $ cmp regX

  0xc0 => imm $ cmp regY
  0xc4 => byVal zp $ cmp regY
  0xcc => byVal abs $ cmp regY

  0xc6 => inplace zp dec
  0xd6 => inplace zpX dec
  0xce => inplace abs dec
  0xde => inplace absX dec
  0xca => implied regX dec
  0x88 => implied regY dec

  0x49 => imm eor
  0x45 => byVal zp eor
  0x55 => byVal zpX eor
  0x4d => byVal abs eor
  0x5d => byVal absX eor
  0x59 => byVal absY eor
  0x41 => byVal xInd eor
  0x51 => byVal indY eor

  0x18 => setFlag carry False
  0x38 => setFlag carry True
  0x58 => setFlag interruptEnable False
  0x78 => setFlag interruptEnable True
  0xb8 => setFlag overflow False
  0xd8 => setFlag decimal False
  0xf8 => setFlag decimal True

  0xe6 => inplace zp inc
  0xf6 => inplace zpX inc
  0xee => inplace abs inc
  0xfe => inplace absX inc
  0xe8 => implied regX inc
  0xc8 => implied regY inc

  0x4c => fetchAddr >>= setReg pc
  0x6c => fetchAddr >>= readMemAddr >>= setReg pc

  0x20 => fetchAddr >>= jsr

  0xa9 => imm $ load regA
  0xa5 => byVal zp $ load regA
  0xb5 => byVal zpX $ load regA
  0xad => byVal abs $ load regA
  0xbd => byVal absX $ load regA
  0xb9 => byVal absY $ load regA
  0xa1 => byVal xInd $ load regA
  0xb1 => byVal indY $ load regA

  0xa2 => imm $ load regX
  0xa6 => byVal zp $ load regX
  0xb6 => byVal zpY $ load regX
  0xae => byVal abs $ load regX
  0xbe => byVal absY $ load regX

  0xa0 => imm $ load regY
  0xa4 => byVal zp $ load regY
  0xb4 => byVal zpY $ load regY
  0xac => byVal abs $ load regY
  0xbc => byVal absY $ load regY

  0x4a => implied regA lsr
  0x46 => inplace zp lsr
  0x56 => inplace zpX lsr
  0x4e => inplace abs lsr
  0x5e => inplace absX lsr

  0xea => pure () -- NOP

  0x09 => imm ora
  0x05 => byVal zp ora
  0x15 => byVal zpX ora
  0x0d => byVal abs ora
  0x1d => byVal absX ora
  0x19 => byVal absY ora
  0x01 => byVal xInd ora
  0x11 => byVal indY ora

  0xaa => transfer regA regX
  0x8a => transfer regX regA
  0xa8 => transfer regA regY
  0x98 => transfer regY regA

  0x2a => implied regA rol
  0x26 => inplace zp rol
  0x36 => inplace zpX rol
  0x2e => inplace abs rol
  0x3e => inplace absX rol

  0x6a => implied regA ror
  0x66 => inplace zp ror
  0x76 => inplace zpX ror
  0x6e => inplace abs ror
  0x7e => inplace absX ror

  -- 0x40 => rti -- TODO
  0x60 => rts

  0xe9 => imm sbc
  0xe5 => byVal zp sbc
  0xf5 => byVal zpX sbc
  0xed => byVal abs sbc
  0xfd => byVal absX sbc
  0xf9 => byVal absY sbc
  0xe1 => byVal xInd sbc
  0xf1 => byVal indY sbc

  0x85 => byRef zp $ store regA
  0x95 => byRef zpX $ store regA
  0x8d => byRef abs $ store regA
  0x9d => byRef absX $ store regA
  0x99 => byRef absY $ store regA
  0x81 => byRef xInd $ store regA
  0x91 => byRef indY $ store regA

  0x86 => byRef zp $ store regX
  0x96 => byRef zpY $ store regX
  0x8e => byRef abs $ store regX

  0x84 => byRef zp $ store regY
  0x94 => byRef zpY $ store regY
  0x8c => byRef abs $ store regY

  0x9a => transfer regX sp
  0xba => transfer sp regX
  0x48 => push =<< getReg regA
  0x68 => setReg regA =<< pop
  0x08 => push =<< (`setBit` 4) <$> getReg status
  0x28 => setReg status =<< pop

  op => assert_total $ do
     pc <- getReg pc
     let addr = pc - 1
     idris_crash $ unwords [hex 4 addr, hex 2 op]
  where
    0 Op : Type -> Type
    Op a = IO a

    0 Addressing : Type
    Addressing = IO Addr

    0 Operand : Type
    Operand = {0 a : Type} -> (Byte -> Op a) -> Op a

    imm : (Byte -> Op a) -> Op a
    imm op = fetch >>= op

    byVal : Addressing -> (Byte -> Op ()) -> Op ()
    byVal addressing op = addressing >>= readMem >>= op

    byRef : Addressing -> (Addr -> Op ()) -> Op ()
    byRef addressing op = addressing >>= op

    inplace : Addressing -> (Byte -> Op Byte) -> Op ()
    inplace addressing op = addressing >>= \addr =>
      readMem addr >>= op >>= writeMem addr

    implied : Reg8 -> (Byte -> Op Byte) -> Op ()
    implied reg op = getReg reg >>= op >>= setReg reg

    zp', abs' : IO Byte -> Addressing
    zp' offset = do
      z <- fetch
      offset <- offset
      pure $ cast $ z + offset
    abs' offset = do
      base <- fetchAddr
      offset <- offset
      pure $ base + cast offset

    zp, zpX, zpY, abs, absX, absY, xInd, indY : Addressing
    zp = zp' $ pure 0
    zpX = zp' $ getReg regX
    zpY = zp' $ getReg regY
    abs = abs' $ pure 0
    absX = abs' $ getReg regX
    absY = abs' $ getReg regY

    xInd = do
      z <- fetch
      offset <- getReg regX
      let ref = cast $ z + offset
      readMemAddr ref

    indY = do
      z <- cast <$> fetch
      offset <- getReg regY
      base <- readMemAddr z
      pure $ base + cast offset

    updateFlags : Byte -> Op Byte
    updateFlags result = do
      setFlag zero $ result .&. 0xff == 0
      setFlag negative $ result `testBit` 7
      pure result

    alu : (Byte -> Byte) -> Byte -> Op Byte
    alu f v = updateFlags $ f v

    signed : (Byte -> Byte -> Bool -> Bits16) -> Byte -> Byte -> Op Byte
    signed f v1 v2 = do
      c0 <- getFlag carry
      let result = f v1 v2 c0

      when ((result `testBit` 7) /= (v1 .&. v2 `testBit` 7)) $ setFlag overflow True

      setFlag carry $ result >= 0x100
      updateFlags $ cast result

    sub : Byte -> Byte -> Op Byte
    sub = signed $ \v1, v2, c0 =>
      -- TODO: BCD
      cast v1 - cast v2 - if c0 then 0 else 1

    cmp : Reg8 -> Byte -> Op ()
    cmp reg v = do
      setFlag carry True
      a <- getReg reg
      ignore $ sub a v

    adc, sbc, and, eor, ora : Byte -> Op ()
    adc v = do
      a <- getReg regA
      -- TODO: BCD
      a' <- signed (\v1, v2, c0 => cast v1 + cast v2 + if c0 then 1 else 0) a v
      setReg regA a'

    sbc v = do
      a <- getReg regA
      setReg regA =<< sub a v

    and v = getReg regA >>= alu (.&. v) >>= setReg regA
    eor v = getReg regA >>= alu (`xor` v) >>= setReg regA
    ora v = getReg regA >>= alu (.|. v) >>= setReg regA

    dec, inc : Byte -> Op Byte
    dec = alu (`subtract` 1)
    inc = alu (+ 1)

    bit : Byte -> Op ()
    bit v = do
      a <- getReg regA

      setFlag zero $ a .&. v == 0
      setFlag negative $ v `testBit` 7
      setFlag overflow $ v `testBit` 6

    load : Reg8 -> Byte -> Op ()
    load reg v = do
      let v' = v
      setFlag zero $ v' == 0
      setFlag negative $ v' `testBit` 7
      setReg reg v'

    store : Reg8 -> Addr -> Op ()
    store reg addr = writeMem addr =<< getReg reg

    jsr : Addr -> Op ()
    jsr addr = do
        curr <- getReg pc
        pushAddr (curr - 1)
        setReg pc addr

    transfer : Reg8 -> Reg8 -> Op ()
    transfer from to = getReg from >>= updateFlags >>= setReg to

    shiftRot : ((Bool, Byte) -> (Bool, Byte)) -> Byte -> Op Byte
    shiftRot f v = do
      c <- getFlag carry
      let (c', v') = f (c, v)
      setFlag carry c'
      updateFlags v'

    asl, lsr, rol, ror : Byte -> Op Byte
    asl = shiftRot $ \(c, v) => (v `testBit` 7, v `shiftL` 1)
    lsr = shiftRot $ \(c, v) => (v `testBit` 0, v `shiftR` 1)
    rol = shiftRot $ \(c, v) => (v `testBit` 7, v `shiftL` 1 .|. (if c then 0x01 else 0x00))
    ror = shiftRot $ \(c, v) => (v `testBit` 0, v `shiftR` 1 .|. (if c then 0x80 else 0x00))

    br : Flag -> Bool -> Op ()
    br flag target = do
      offset <- cast <$> fetch
      b <- getFlag flag
      when (b == target) $ do
        ignore $ modifyReg pc $ \pc => pc + offset - if offset < 0x80 then 0 else 0x100
