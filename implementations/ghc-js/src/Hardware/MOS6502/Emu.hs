{-# LANGUAGE RecordWildCards, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hardware.MOS6502.Emu where

import Data.Bits
-- import Control.Monad.IO.Fast
import Data.IORef
import Data.Word
import Control.Monad (void, when)
import Control.Monad.Reader
import Text.Printf

type Byte = Word8
type Addr = Word16

class (MonadIO m) => MonadMachine m where
    readMem :: Addr -> m Byte
    writeMem :: Addr -> Byte -> m ()

instance (MonadMachine m) => MonadMachine (ReaderT r m) where
    readMem = lift . readMem
    writeMem addr = lift . writeMem addr

data CPU = MkCPU
  { regA, regX, regY, status, sp :: IORef Byte
  , pc :: IORef Addr
  }

new :: Addr -> IO CPU
new pc0 = do
    regA   <- newIORef 0x00
    regX   <- newIORef 0x00
    regY   <- newIORef 0x00
    status <- newIORef 0x00
    sp     <- newIORef 0xff
    pc     <- newIORef pc0
    pure MkCPU{..}

type Reg a = CPU -> IORef a

getReg :: (MonadIO m) => Reg a -> ReaderT CPU m a
getReg reg = do
    r <- asks reg
    liftIO $ readIORef r

setReg :: (MonadIO m) => Reg a -> a -> ReaderT CPU m ()
setReg reg v = do
    r <- asks reg
    liftIO $ writeIORef r v

modifyReg :: (MonadIO m) => Reg a -> (a -> a) -> ReaderT CPU m a
modifyReg reg f = do
    v <- getReg reg
    setReg reg $ f v
    pure v

fetch :: (MonadMachine m) => ReaderT CPU m Byte
fetch = readMem =<< modifyReg pc (+ 1)

toAddr :: Byte -> Byte -> Addr
toAddr lo hi = (fromIntegral hi) `shiftL` 8 .|. fromIntegral lo

fetchAddr :: (MonadMachine m) => ReaderT CPU m Addr
fetchAddr = toAddr <$> fetch <*> fetch

readMemAddr :: (MonadMachine m) => Addr -> ReaderT CPU m Addr
readMemAddr addr = toAddr <$> readMem addr <*> readMem (addr + 1)

push :: (MonadMachine m) => Byte -> ReaderT CPU m ()
push v = do
    ptr <- modifyReg sp (subtract 1)
    writeMem (0x0100 + fromIntegral ptr) v

pushAddr :: (MonadMachine m) => Addr -> ReaderT CPU m ()
pushAddr addr = push hi >> push lo
  where
    hi = fromIntegral $ addr `shiftR` 8
    lo = fromIntegral addr

pop :: (MonadMachine m) => ReaderT CPU m Byte
pop = do
  ptr <- modifyReg sp (+ 1)
  readMem (0x0100 + fromIntegral (ptr + 1))

popAddr :: (MonadMachine m) => ReaderT CPU m Addr
popAddr = toAddr <$> pop <*> pop

type Flag = Int

getFlag :: (MonadIO m) => Flag -> ReaderT CPU m Bool
getFlag flag = do
  flags <- getReg status
  pure $ flags `testBit` flag

setFlag :: (MonadIO m) => Flag -> Bool -> ReaderT CPU m ()
setFlag flag b = void $ modifyReg status $ flip (if b then setBit else clearBit) flag

carry, zero, interruptEnable, decimal, overflow, negative :: Flag
carry = 0
zero = 1
interruptEnable = 2
decimal = 3
overflow = 6
negative = 7

rts :: (MonadMachine m) => ReaderT CPU m ()
rts = do
    addr <- popAddr
    setReg pc (addr + 1)

step :: forall m. (MonadMachine m) => ReaderT CPU m ()
step = fetch >>= \case -- http://www.6502.org/tutorials/6502opcodes.html
  0x69 -> imm adc
  0x65 -> byVal zp adc
  0x75 -> byVal zpX adc
  0x6d -> byVal abs adc
  0x7d -> byVal absX adc
  0x79 -> byVal absY adc
  0x61 -> byVal xInd adc
  0x71 -> byVal indY adc

  0x29 -> imm and
  0x25 -> byVal zp and
  0x35 -> byVal zpX and
  0x2d -> byVal abs and
  0x3d -> byVal absX and
  0x39 -> byVal absY and
  0x21 -> byVal xInd and
  0x31 -> byVal indY and

  0x0a -> implied regA asl
  0x06 -> inplace zp asl
  0x16 -> inplace zpX asl
  0x0e -> inplace abs asl
  0x1e -> inplace absX asl

  0x24 -> byVal zp bit
  0x2c -> byVal abs bit

  0x10 -> br negative False
  0x30 -> br negative True
  0x50 -> br overflow False
  0x70 -> br overflow True
  0x90 -> br carry False
  0xb0 -> br carry True
  0xd0 -> br zero False
  0xf0 -> br zero True

--   -- 0x00 -> brk -- TODO

  0xc9 -> imm $ cmp regA
  0xc5 -> byVal zp $ cmp regA
  0xd5 -> byVal zpX $ cmp regA
  0xcd -> byVal abs $ cmp regA
  0xdd -> byVal absX $ cmp regA
  0xd9 -> byVal absY $ cmp regA
  0xc1 -> byVal xInd $ cmp regA
  0xd1 -> byVal indY $ cmp regA

  0xe0 -> imm $ cmp regX
  0xe4 -> byVal zp $ cmp regX
  0xec -> byVal abs $ cmp regX

  0xc0 -> imm $ cmp regY
  0xc4 -> byVal zp $ cmp regY
  0xcc -> byVal abs $ cmp regY

  0xc6 -> inplace zp dec
  0xd6 -> inplace zpX dec
  0xce -> inplace abs dec
  0xde -> inplace absX dec
  0xca -> implied regX dec
  0x88 -> implied regY dec

  0x49 -> imm eor
  0x45 -> byVal zp eor
  0x55 -> byVal zpX eor
  0x4d -> byVal abs eor
  0x5d -> byVal absX eor
  0x59 -> byVal absY eor
  0x41 -> byVal xInd eor
  0x51 -> byVal indY eor

  0x18 -> setFlag carry False
  0x38 -> setFlag carry True
  0x58 -> setFlag interruptEnable False
  0x78 -> setFlag interruptEnable True
  0xb8 -> setFlag overflow False
  0xd8 -> setFlag decimal False
  0xf8 -> setFlag decimal True

  0xe6 -> inplace zp inc
  0xf6 -> inplace zpX inc
  0xee -> inplace abs inc
  0xfe -> inplace absX inc
  0xe8 -> implied regX inc
  0xc8 -> implied regY inc

  0x4c -> fetchAddr >>= setReg pc
  0x6c -> fetchAddr >>= readMemAddr >>= setReg pc

  0x20 -> fetchAddr >>= jsr

  0xa9 -> imm $ load regA
  0xa5 -> byVal zp $ load regA
  0xb5 -> byVal zpX $ load regA
  0xad -> byVal abs $ load regA
  0xbd -> byVal absX $ load regA
  0xb9 -> byVal absY $ load regA
  0xa1 -> byVal xInd $ load regA
  0xb1 -> byVal indY $ load regA

  0xa2 -> imm $ load regX
  0xa6 -> byVal zp $ load regX
  0xb6 -> byVal zpY $ load regX
  0xae -> byVal abs $ load regX
  0xbe -> byVal absY $ load regX

  0xa0 -> imm $ load regY
  0xa4 -> byVal zp $ load regY
  0xb4 -> byVal zpY $ load regY
  0xac -> byVal abs $ load regY
  0xbc -> byVal absY $ load regY

  0x4a -> implied regA lsr
  0x46 -> inplace zp lsr
  0x56 -> inplace zpX lsr
  0x4e -> inplace abs lsr
  0x5e -> inplace absX lsr

  0xea -> pure () -- NOP

  0x09 -> imm ora
  0x05 -> byVal zp ora
  0x15 -> byVal zpX ora
  0x0d -> byVal abs ora
  0x1d -> byVal absX ora
  0x19 -> byVal absY ora
  0x01 -> byVal xInd ora
  0x11 -> byVal indY ora

  0xaa -> transfer regA regX
  0x8a -> transfer regX regA
  0xa8 -> transfer regA regY
  0x98 -> transfer regY regA

  0x2a -> implied regA rol
  0x26 -> inplace zp rol
  0x36 -> inplace zpX rol
  0x2e -> inplace abs rol
  0x3e -> inplace absX rol

  0x6a -> implied regA ror
  0x66 -> inplace zp ror
  0x76 -> inplace zpX ror
  0x6e -> inplace abs ror
  0x7e -> inplace absX ror

  -- 0x40 -> rti -- TODO
  0x60 -> rts

  0xe9 -> imm sbc
  0xe5 -> byVal zp sbc
  0xf5 -> byVal zpX sbc
  0xed -> byVal abs sbc
  0xfd -> byVal absX sbc
  0xf9 -> byVal absY sbc
  0xe1 -> byVal xInd sbc
  0xf1 -> byVal indY sbc

  0x85 -> byRef zp $ store regA
  0x95 -> byRef zpX $ store regA
  0x8d -> byRef abs $ store regA
  0x9d -> byRef absX $ store regA
  0x99 -> byRef absY $ store regA
  0x81 -> byRef xInd $ store regA
  0x91 -> byRef indY $ store regA

  0x86 -> byRef zp $ store regX
  0x96 -> byRef zpY $ store regX
  0x8e -> byRef abs $ store regX

  0x84 -> byRef zp $ store regY
  0x94 -> byRef zpY $ store regY
  0x8c -> byRef abs $ store regY

  0x9a -> transfer regX sp
  0xba -> transfer sp regX
  0x48 -> push =<< getReg regA
  0x68 -> setReg regA =<< pop
  0x08 -> push =<< (`setBit` 4) <$> getReg status
  0x28 -> setReg status =<< pop

  op -> do
     pc <- getReg pc
     let addr = pc - 1
     error $ printf "0x%04x: Unknown instruction 0x%02x" addr op
  where
    imm op = fetch >>= op
    byVal addressing op = addressing >>= readMem >>= op
    byRef addressing op = addressing >>= op
    inplace addressing op = addressing >>= \addr -> readMem addr >>= op >>= writeMem addr
    implied reg op = getReg reg >>= op >>= setReg reg

    zp' offset = do
      z <- fetch
      offset <- offset
      pure $ fromIntegral $ z + offset
    abs' offset = do
      base <- fetchAddr
      offset <- offset
      pure $ base + fromIntegral offset

    zp = zp' $ pure 0
    zpX = zp' $ getReg regX
    zpY = zp' $ getReg regY
    abs = abs' $ pure 0
    absX = abs' $ getReg regX
    absY = abs' $ getReg regY

    xInd = do
      z <- fetch
      offset <- getReg regX
      let ref = fromIntegral $ z + offset
      readMemAddr ref

    indY = do
      z <- fromIntegral <$> fetch
      offset <- getReg regY
      base <- readMemAddr z
      pure $ base + fromIntegral offset

    updateFlags result = do
      setFlag zero $ result .&. 0xff == 0
      setFlag negative $ result `testBit` 7
      pure result

    alu f v = updateFlags $ f v

    signed :: (Byte -> Byte -> Bool -> Addr) -> Byte -> Byte -> ReaderT CPU m Byte
    signed f v1 v2 = do
      c0 <- getFlag carry
      let result = f v1 v2 c0

      when ((result `testBit` 7) /= ((v1 .&. v2) `testBit` 7)) $ setFlag overflow True

      setFlag carry $ result >= 0x100
      updateFlags $ fromIntegral result

    sub = signed $ \v1 v2 c0 ->
      -- TODO: BCD
      fromIntegral v1 - fromIntegral v2 - if c0 then 0 else 1

    cmp reg v = do
      setFlag carry True
      a <- getReg reg
      void $ sub a v

    adc v = do
      a <- getReg regA
      -- TODO: BCD
      a' <- signed (\v1 v2 c0 -> fromIntegral v1 + fromIntegral v2 + if c0 then 1 else 0) a v
      setReg regA a'

    sbc v = do
      a <- getReg regA
      setReg regA =<< sub a v

    and v = getReg regA >>= alu (.&. v) >>= setReg regA
    eor v = getReg regA >>= alu (`xor` v) >>= setReg regA
    ora v = getReg regA >>= alu (.|. v) >>= setReg regA

    dec = alu (subtract 1)
    inc = alu (+ 1)

    bit v = do
      a <- getReg regA

      setFlag zero $ a .&. v == 0
      setFlag negative $ v `testBit` 7
      setFlag overflow $ v `testBit` 6

    load reg v = do
      let v' = v
      setFlag zero $ v' == 0
      setFlag negative $ v' `testBit` 7
      setReg reg v'

    store reg addr = writeMem addr =<< getReg reg

    jsr addr = do
        curr <- getReg pc
        pushAddr (curr - 1)
        setReg pc addr

    transfer from to = getReg from >>= updateFlags >>= setReg to

    shiftRot f v = do
      c <- getFlag carry
      let (c', v') = f (c, v)
      setFlag carry c'
      updateFlags v'

    asl = shiftRot $ \(c, v) -> (v `testBit` 7, v `shiftL` 1)
    lsr = shiftRot $ \(c, v) -> (v `testBit` 0, v `shiftR` 1)
    rol = shiftRot $ \(c, v) -> (v `testBit` 7, v `shiftL` 1 .|. (if c then 0x01 else 0x00))
    ror = shiftRot $ \(c, v) -> (v `testBit` 0, v `shiftR` 1 .|. (if c then 0x80 else 0x00))

    br flag target = do
      offset <- fromIntegral <$> fetch
      b <- getFlag flag
      when (b == target) $ do
        void $ modifyReg pc $ \pc -> pc + offset - if offset < 0x80 then 0 else 0x100
