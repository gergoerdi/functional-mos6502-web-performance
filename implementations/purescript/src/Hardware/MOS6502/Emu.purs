module Hardware.MOS6502.Emu where

import Prelude
import Data.Word
import Data.Integral (class Integral, fromIntegral)
import Data.UInt (fromInt)
import Data.Int (toStringAs, hexadecimal)
import Data.Shift
import Data.String (joinWith, length)
import Data.Array (replicate)

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Uncurried.ReaderT

import Data.Lens
import Partial

type Addr = Word16

class (MonadEffect m) <= MonadMachine m where
    readMem :: Addr -> m Word8
    writeMem :: Addr -> Word8 -> m Unit

instance (MonadMachine m) => MonadMachine (ReaderT r m) where
    readMem = lift <<< readMem
    writeMem addr = lift <<< writeMem addr

type CPU = 
    { regA :: Ref Word8
    , regX :: Ref Word8
    , regY :: Ref Word8
    , status :: Ref Word8
    , sp :: Ref Word8
    , pc :: Ref Addr
    }

new :: forall m. MonadEffect m => Addr -> m CPU
new pc0 = liftEffect do
    regA <- Ref.new <<< fromIntegral $ 0x00
    regX <- Ref.new <<< fromIntegral $ 0x00
    regY <- Ref.new <<< fromIntegral $ 0x00
    status <- Ref.new <<< fromIntegral $ 0x00
    sp <- Ref.new <<< fromIntegral $ 0xff
    pc <- Ref.new pc0
    pure { regA: regA, regX: regX, regY: regY, status: status, sp: sp, pc: pc }

fetch :: forall m. (MonadMachine m) => ReaderT CPU m Word8
fetch = do
    pc <- asks _.pc
    addr <- liftEffect $ Ref.read pc
    liftEffect $ Ref.write (addr + fromIntegral 1) pc
    readMem addr

toAddr :: Word8 -> Word8 -> Addr
toAddr lo hi = (fromIntegral hi `shl` fromInt 8)  .|. fromIntegral lo

fetchAddr :: forall m. (MonadMachine m) => ReaderT CPU m Addr
fetchAddr = toAddr <$> fetch <*> fetch

readMemAddr :: forall m. (MonadMachine m) => Addr -> m Addr
readMemAddr addr = toAddr <$> readMem addr <*> readMem (addr + fromIntegral 1)

getReg :: forall m a. (MonadEffect m) => (CPU -> Ref a) -> ReaderT CPU m a
getReg reg = do
    ref <- asks reg
    liftEffect $ Ref.read ref

setReg :: forall m a. (MonadEffect m) => (CPU -> Ref a) -> a -> ReaderT CPU m Unit
setReg reg v = do
    ref <- asks reg
    liftEffect $ Ref.write v ref

modifyReg :: forall m a. (MonadEffect m) => (CPU -> Ref a) -> (a -> a) -> ReaderT CPU m Unit
modifyReg reg f = setReg reg <<< f =<< getReg reg

push :: forall m. (MonadMachine m) => Word8 -> ReaderT CPU m Unit
push v = do
    ptr <- getReg _.sp
    writeMem (fromIntegral 0x100 + fromIntegral ptr) v
    setReg _.sp $ ptr - fromIntegral 1

pushAddr :: forall m. (MonadMachine m) => Addr -> ReaderT CPU m Unit
pushAddr addr = push hi *> push lo
  where
    hi = fromIntegral $ addr `shr` fromInt 8
    lo = fromIntegral $ addr .&. fromIntegral 0xff

pop :: forall m. (MonadMachine m) => ReaderT CPU m Word8
pop = do
    ptr <- getReg _.sp
    v <- readMem (fromIntegral 0x100 + fromIntegral (ptr + fromIntegral 1))
    setReg _.sp $ ptr + fromIntegral 1
    pure v

popAddr :: forall m. (MonadMachine m) => ReaderT CPU m Addr
popAddr = toAddr <$> pop <*> pop

getFlag :: forall m. (MonadEffect m) => (Lens' Word8 Boolean) -> ReaderT CPU m Boolean
getFlag flag = do
    ref <- asks _.status
    flags <- liftEffect $ Ref.read ref
    pure $ flags ^. flag

setFlag :: forall m. (MonadEffect m) => (Lens' Word8 Boolean) -> Boolean -> ReaderT CPU m Unit
setFlag flag b = do
    ref <- asks _.status
    void $ liftEffect $ Ref.modify (set flag b) ref

carry :: Lens' Word8 Boolean
carry = statusFlag 0

zero :: Lens' Word8 Boolean
zero = statusFlag 1

interruptEnable :: Lens' Word8 Boolean
interruptEnable = statusFlag 2

decimal :: Lens' Word8 Boolean
decimal = statusFlag 3

overflow :: Lens' Word8 Boolean
overflow = statusFlag 6

negative :: Lens' Word8 Boolean
negative = statusFlag 7

mask :: forall a. Shift a => Integral a => Int -> a
mask i = fromIntegral 1 `shl` fromInt i

testBit :: forall a. HeytingAlgebra a => Shift a => Integral a => Int -> a -> Boolean
testBit i x = (x .&. mask i) /= fromIntegral 0

setBit :: forall a. HeytingAlgebra a => Shift a => Integral a => Int -> a -> Boolean -> a
setBit i x b = if b then x .|. mask i else x .&. not (mask i)

xor :: forall a. HeytingAlgebra a => a -> a -> a
xor a b = (a .|. b) .&. not (a .&. b)

statusFlag :: Int -> Lens' Word8 Boolean
statusFlag i = lens (testBit i) (setBit i)

hex :: forall a. Integral a => Int -> a -> String
hex n = pad n <<< toStringAs hexadecimal <<< fromIntegral
  where
    pad :: Int -> String -> String
    pad n s = "0x" <> joinWith "" (replicate k "0") <> s
      where
        k = max 0 $ n - length s

step :: forall m. MonadMachine m => Partial => ReaderT CPU m Unit
step = fetch >>= \op -> case fromIntegral op of -- http://www.6502.org/tutorials/6502opcodes.html
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

    0x0a -> implied _.regA asl
    0x06 -> inplace zp asl
    0x16 -> inplace zpX asl
    0x0e -> inplace abs asl
    0x1e -> inplace absX asl

    0x24 -> byVal zp bit
    0x2c -> byVal abs bit

    0x10 -> br negative false
    0x30 -> br negative true
    0x50 -> br overflow false
    0x70 -> br overflow true
    0x90 -> br carry false
    0xb0 -> br carry true
    0xd0 -> br zero false
    0xf0 -> br zero true

    -- 0x00 -> brk -- TODO

    0xc9 -> imm $ cmp _.regA
    0xc5 -> byVal zp $ cmp _.regA
    0xd5 -> byVal zpX $ cmp _.regA
    0xcd -> byVal abs $ cmp _.regA
    0xdd -> byVal absX $ cmp _.regA
    0xd9 -> byVal absY $ cmp _.regA
    0xc1 -> byVal xInd $ cmp _.regA
    0xd1 -> byVal indY $ cmp _.regA

    0xe0 -> imm $ cmp _.regX
    0xe4 -> byVal zp $ cmp _.regX
    0xec -> byVal abs $ cmp _.regX

    0xc0 -> imm $ cmp _.regY
    0xc4 -> byVal zp $ cmp _.regY
    0xcc -> byVal abs $ cmp _.regY

    0xc6 -> inplace zp dec
    0xd6 -> inplace zpX dec
    0xce -> inplace abs dec
    0xde -> inplace absX dec
    0xca -> implied _.regX dec
    0x88 -> implied _.regY dec

    0x49 -> imm eor
    0x45 -> byVal zp eor
    0x55 -> byVal zpX eor
    0x4d -> byVal abs eor
    0x5d -> byVal absX eor
    0x59 -> byVal absY eor
    0x41 -> byVal xInd eor
    0x51 -> byVal indY eor

    0x18 -> setFlag carry false
    0x38 -> setFlag carry true
    0x58 -> setFlag interruptEnable false
    0x78 -> setFlag interruptEnable true
    0xb8 -> setFlag overflow false
    0xd8 -> setFlag decimal false
    0xf8 -> setFlag decimal true

    0xe6 -> inplace zp inc
    0xf6 -> inplace zpX inc
    0xee -> inplace abs inc
    0xfe -> inplace absX inc
    0xe8 -> implied _.regX inc
    0xc8 -> implied _.regY inc

    0x4c -> fetchAddr >>= setReg _.pc
    0x6c -> fetchAddr >>= readMemAddr >>= setReg _.pc

    0x20 -> fetchAddr >>= jsr

    0xa9 -> imm $ load _.regA
    0xa5 -> byVal zp $ load _.regA
    0xb5 -> byVal zpX $ load _.regA
    0xad -> byVal abs $ load _.regA
    0xbd -> byVal absX $ load _.regA
    0xb9 -> byVal absY $ load _.regA
    0xa1 -> byVal xInd $ load _.regA
    0xb1 -> byVal indY $ load _.regA

    0xa2 -> imm $ load _.regX
    0xa6 -> byVal zp $ load _.regX
    0xb6 -> byVal zpY $ load _.regX
    0xae -> byVal abs $ load _.regX
    0xbe -> byVal absY $ load _.regX

    0xa0 -> imm $ load _.regY
    0xa4 -> byVal zp $ load _.regY
    0xb4 -> byVal zpY $ load _.regY
    0xac -> byVal abs $ load _.regY
    0xbc -> byVal absY $ load _.regY

    0x4a -> implied _.regA lsr
    0x46 -> inplace zp lsr
    0x56 -> inplace zpX lsr
    0x4e -> inplace abs lsr
    0x5e -> inplace absX lsr

    0xea -> pure unit -- NOP

    0x09 -> imm ora
    0x05 -> byVal zp ora
    0x15 -> byVal zpX ora
    0x0d -> byVal abs ora
    0x1d -> byVal absX ora
    0x19 -> byVal absY ora
    0x01 -> byVal xInd ora
    0x11 -> byVal indY ora

    0xaa -> transfer _.regA _.regX
    0x8a -> transfer _.regX _.regA
    0xa8 -> transfer _.regA _.regY
    0x98 -> transfer _.regY _.regA

    0x2a -> implied _.regA rol
    0x26 -> inplace zp rol
    0x36 -> inplace zpX rol
    0x2e -> inplace abs rol
    0x3e -> inplace absX rol

    0x6a -> implied _.regA ror
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

    0x85 -> byRef zp $ store _.regA
    0x95 -> byRef zpX $ store _.regA
    0x8d -> byRef abs $ store _.regA
    0x9d -> byRef absX $ store _.regA
    0x99 -> byRef absY $ store _.regA
    0x81 -> byRef xInd $ store _.regA
    0x91 -> byRef indY $ store _.regA

    0x86 -> byRef zp $ store _.regX
    0x96 -> byRef zpY $ store _.regX
    0x8e -> byRef abs $ store _.regX

    0x84 -> byRef zp $ store _.regY
    0x94 -> byRef zpY $ store _.regY
    0x8c -> byRef abs $ store _.regY

    0x9a -> transfer _.regX _.sp
    0xba -> transfer _.sp _.regX
    0x48 -> push =<< getReg _.regA
    0x68 -> setReg _.regA =<< pop
    0x08 -> push <<< (\v -> setBit 4 v true) =<< getReg _.status
    0x28 -> setReg _.status =<< pop

    op -> do
        pc <- getReg _.pc
        let addr = pc - fromIntegral 1
        crashWith $ hex 4 addr <> " " <> hex 2 op
  where
    byVal addressing op = do
        addr <- addressing
        val <- readMem addr
        op val

    byRef addressing op = addressing >>= op

    inplace addressing op = byRef addressing \addr -> do
        writeMem addr =<< op =<< readMem addr

    implied reg op = setReg reg =<< op =<< getReg reg

    zp' offset = do
        z <- fetch
        offset <- offset
        pure $ fromIntegral $ z + offset

    abs' offset = do
        base <- fetchAddr
        offset <- offset
        pure $ base + fromIntegral offset

    imm op = fetch >>= op
    zp = zp' (pure $ fromIntegral 0)
    zpX = zp' (getReg _.regX)
    zpY = zp' (getReg _.regY)
    abs = abs' (pure $ fromIntegral 0)
    absX = abs' (getReg _.regX)
    absY = abs' (getReg _.regY)

    xInd = do
        z <- fetch
        offset <- getReg _.regX
        let ref = fromIntegral $ z + offset
        readMemAddr ref

    indY = do
        z <- fromIntegral <$> fetch
        offset <- getReg _.regY
        base <- readMemAddr z
        pure $ base + fromIntegral offset

    signed f v1 v2 = do
        c0 <- getFlag carry
        let result = f v1 v2 c0
        -- TODO: BCD

        when (testBit 7 result /= testBit 7 (v1 .&. v2)) do
            setFlag overflow true

        setFlag carry $ result >= fromIntegral 0x100
        setFlag zero $ result .&. fromIntegral 0xff == fromIntegral 0
        setFlag negative $ testBit 7 result
        pure (fromIntegral result :: Word8)

    adc v = do
        a <- getReg _.regA
        a' <- signed (\v1 v2 c0 -> fromIntegral v1 + fromIntegral v2 + fromIntegral (if c0 then 1 else 0) :: Word16) a v
        setReg _.regA a'

    sub v1 v2 = do
        c0 <- getFlag carry
        let extended = fromIntegral v1 - fromIntegral v2 - fromIntegral (if c0 then 0 else 1) :: Word16
        -- TODO: BCD

        when (testBit 7 extended /= (testBit 7 v1 && testBit 7 v2)) do
            setFlag overflow true

        setFlag carry $ extended <= fromIntegral 0x100
        setFlag zero $ extended .&. fromIntegral 0xff == fromIntegral 0
        setFlag negative $ testBit 7 extended
        pure $ fromIntegral extended

    cmp reg v = do
        setFlag carry true
        a <- getReg reg
        void $ sub a v

    sbc v = do
        a <- getReg _.regA
        setReg _.regA =<< sub a v

    alu f a = do
        let result = f a

        setFlag zero $ result .&. fromIntegral 0xff == fromIntegral 0
        setFlag negative $ testBit 7 result
        pure result

    and v = setReg _.regA =<< alu (_ .&. v) =<< getReg _.regA
    eor v = setReg _.regA =<< alu (_ `xor` v) =<< getReg _.regA
    ora v = setReg _.regA =<< alu (_ .|. v) =<< getReg _.regA

    shiftRot f v = do
        c <- getFlag carry
        let {c' : c', v': v'} = f c v

        setFlag carry $ c'
        setFlag zero $ v' == fromIntegral 0
        setFlag negative $ testBit 7 v'
        pure v'

    asl = shiftRot \c v -> { c': testBit 7 v, v': v `shl` fromInt 1 }
    lsr = shiftRot \c v -> { c': testBit 0 v, v': v `shr` fromInt 1 }
    rol = shiftRot \c v -> { c': testBit 7 v, v': setBit 0 (v `shl` fromInt 1) c }
    ror = shiftRot \c v -> { c' : testBit 0 v, v' : setBit 7 (v `shr` fromInt 1) c }

    bit v = do
        a <- getReg _.regA

        setFlag zero $ a .&. v == fromIntegral 0
        setFlag negative $ testBit 7 v
        setFlag overflow $ testBit 6 v 

    br :: Lens' Word8 Boolean -> Boolean -> ReaderT CPU m Unit
    br flag target = do
        offset <- fromIntegral <$> fetch
        b <- getFlag flag
        when (b == target) do
            modifyReg _.pc $ if offset < 0x80 then (\pc -> pc + fromIntegral offset) 
                else (\pc -> pc + fromIntegral offset - fromIntegral 256)

    dec = alu (_ - fromIntegral 1)
    inc = alu (_ + fromIntegral 1)

    load reg v = do
        let v' = v
        setFlag zero $ v' == fromIntegral 0
        setFlag negative $ testBit 7 v'
        setReg reg v'

    store reg addr = writeMem addr =<< getReg reg

    jsr addr = do
        curr <- getReg _.pc
        pushAddr (curr - fromIntegral 1)
        setReg _.pc addr

    transfer from to = do
        v <- getReg from
        setReg to =<< alu (const v) (fromIntegral 0)

rts :: forall m. (MonadMachine m) => ReaderT CPU m Unit
rts = do
    addr <- popAddr
    setReg _.pc (addr + fromIntegral 1)
