module mos6502

import StdEnv
import StdMaybe

import ABC.Interpreter.JavaScript

Start = wrapInitFunction start

start :: !JSVal !*JSWorld -> *JSWorld
start me w
	# (cb,w) = jsWrapFunWithResult (run me) me w
	# w = (jsGlobal "clean_run" .= cb) w
	= w

run :: !JSVal !{!JSVal} !*JSWorld -> (!JSVal, !*JSWorld)
run me {[0]=arrayBuffer} w
	# (js_array,w) = jsNew "Uint8Array" arrayBuffer w
	  (js_array,w) = (jsGlobal "Array" .# "from" .$ js_array) w
	  (len,w) = js_array .# "length" .? w
	  len = fromJust (jsValToMaybe len)
	  (array,w) = copyArray 0 len (createArray len (Byte 0)) js_array w
	# cpu = new (Addr 0x438b) array
	# cnt = loop 0 cpu
	= (toJS cnt, w)
where
	copyArray :: !Int !Int !*{#Byte} !JSVal !*JSWorld -> (!*{#Byte}, !*JSWorld)
	copyArray i len array js_array w
		| i >= len = (array, w)
		# (x,w) = js_array .# i .? w
		  array & [i] = Byte (fromJust (jsValToMaybe x))
		= copyArray (i+1) len array js_array w

	loop cnt cpu
		# (pc,cpu) = cpu!pc
		| pc=:(Addr 0x640b) = cnt
		# cpu = step cpu
		= loop (cnt+1) cpu

:: Addr =: Addr Int

class toAddr a :: !a -> Addr
instance toAddr Byte where toAddr (Byte x) = Addr x
instance toAddr Int where toAddr x = Addr (x bitand 0xffff)

instance toInt Addr where toInt (Addr x) = x
instance + Addr where (+) (Addr x) (Addr y) = Addr ((x+y) bitand 0xffff)
instance - Addr where (-) (Addr x) (Addr y) = Addr ((x-y) bitand 0xffff)

:: Byte =: Byte Int

class toByte a :: !a -> Byte
instance toByte Addr where toByte (Addr x) = Byte (x bitand 0xff)
instance toByte Int where toByte x = Byte (x bitand 0xffff)

instance toInt Byte where toInt (Byte x) = x
instance + Byte where (+) (Byte x) (Byte y) = Byte ((x+y) bitand 0xff)
instance - Byte where (-) (Byte x) (Byte y) = Byte ((x-y) bitand 0xff)

:: *CPU =
	{ pc :: !Addr
	, sp :: !Byte
	, status :: !Byte
	, regA :: !Byte
	, regX :: !Byte
	, regY :: !Byte
	, mem :: !*{#Byte}
	}

new :: !Addr !*{#Byte} -> CPU
new pc mem =
	{ pc = pc
	, sp = Byte 0xff
	, status = Byte 0
	, regA = Byte 0
	, regX = Byte 0
	, regY = Byte 0
	, mem = mem
	}

fetch :: !CPU -> (!Byte, !CPU)
fetch cpu=:{pc=Addr pc}
	# cpu & pc = Addr (pc+1)
	= cpu!mem.[pc]

fetchAddr :: !CPU -> (!Addr, !CPU)
fetchAddr cpu
	# (l,cpu) = fetch cpu
	# (h,cpu) = fetch cpu
	= (Addr ((toInt h << 8) bitor toInt l), cpu)

readMem :: !Addr !CPU -> (!Byte, !CPU)
readMem (Addr addr) cpu = cpu!mem.[addr]

writeMem :: !Addr !Byte !CPU -> CPU
writeMem (Addr addr) byte cpu = {cpu & mem.[addr]=byte}

readMemAddr :: !Addr !CPU -> (!Addr, !CPU)
readMemAddr addr cpu
	# (l,cpu) = readMem addr cpu
	# (h,cpu) = readMem (addr + Addr 1) cpu
	= (Addr ((toInt h << 8) bitor toInt l), cpu)

push :: !Byte !CPU -> CPU
push v cpu=:{sp}
	# cpu = writeMem (Addr 0x0100 + toAddr sp) v cpu
	  cpu & sp = sp - Byte 1
	= cpu

pushAddr :: !Addr !CPU -> CPU
pushAddr addr cpu
	# cpu = push hi cpu
	= push lo cpu
where
	hi = toByte (toInt addr >> 8)
	lo = toByte addr

pop :: !CPU -> (!Byte, !CPU)
pop cpu=:{sp}
	# (v,cpu) = readMem (Addr 0x0101 + toAddr sp) cpu
	  cpu & sp = sp + Byte 1
	= (v, cpu)

popAddr :: !CPU -> (!Addr, !CPU)
popAddr cpu
	# (l,cpu) = pop cpu
	# (h,cpu) = pop cpu
	= (Addr ((toInt h << 8) bitor toInt l), cpu)

rts :: !CPU -> CPU
rts cpu
	# (addr,cpu) = popAddr cpu
	  cpu & pc = addr + Addr 1
	= cpu

:: Flag :== Byte

setFlag :: !Flag !Bool !CPU -> CPU
setFlag flag b cpu=:{status} =
	{ cpu
	& status = toByte (if b
		(toInt status bitor toInt flag)
		(toInt status bitand (~(toInt flag) - 1)))
	}

getFlag :: !Flag !CPU -> (!Bool, !CPU)
getFlag flag cpu=:{status} = (toInt status bitand toInt flag <> 0, cpu)

carry :== Byte 0x01
zero :== Byte 0x02
interruptEnable :== Byte 0x04
decimal :== Byte 0x08
overflow :== Byte 0x40
negative :== Byte 0x80

step :: !CPU -> CPU
step cpu=:{pc}
	# (instr,cpu) = fetch cpu
	= case toInt instr of // http://www.6502.org/tutorials/6502opcodes.html
		0x69 -> imm adc cpu
		0x65 -> byVal zp adc cpu
		0x75 -> byVal zpX adc cpu
		0x6d -> byVal abs adc cpu
		0x7d -> byVal absX adc cpu
		0x79 -> byVal absY adc cpu
		0x61 -> byVal xInd adc cpu
		0x71 -> byVal indY adc cpu

		0x29 -> imm and cpu
		0x25 -> byVal zp and cpu
		0x35 -> byVal zpX and cpu
		0x2d -> byVal abs and cpu
		0x3d -> byVal absX and cpu
		0x39 -> byVal absY and cpu
		0x21 -> byVal xInd and cpu
		0x31 -> byVal indY and cpu

		0x0a -> implied (\cpu -> cpu!regA) (\v cpu -> {cpu & regA=v}) asl cpu
		0x06 -> inplace zp asl cpu
		0x16 -> inplace zpX asl cpu
		0x0e -> inplace abs asl cpu
		0x1e -> inplace absX asl cpu

		0x24 -> byVal zp bit cpu
		0x2c -> byVal abs bit cpu

		0x10 -> br negative False cpu
		0x30 -> br negative True cpu
		0x50 -> br overflow False cpu
		0x70 -> br overflow True cpu
		0x90 -> br carry False cpu
		0xb0 -> br carry True cpu
		0xd0 -> br zero False cpu
		0xf0 -> br zero True cpu

		// 0x00 -> brk // TODO

		0xc9 -> imm (cmp \cpu -> cpu!regA) cpu
		0xc5 -> byVal zp (cmp \cpu -> cpu!regA) cpu
		0xd5 -> byVal zpX (cmp \cpu -> cpu!regA) cpu
		0xcd -> byVal abs (cmp \cpu -> cpu!regA) cpu
		0xdd -> byVal absX (cmp \cpu -> cpu!regA) cpu
		0xd9 -> byVal absY (cmp \cpu -> cpu!regA) cpu
		0xc1 -> byVal xInd (cmp \cpu -> cpu!regA) cpu
		0xd1 -> byVal indY (cmp \cpu -> cpu!regA) cpu

		0xe0 -> imm (cmp \cpu -> cpu!regX) cpu
		0xe4 -> byVal zp (cmp \cpu -> cpu!regX) cpu
		0xec -> byVal abs (cmp \cpu -> cpu!regX) cpu

		0xc0 -> imm (cmp \cpu -> cpu!regY) cpu
		0xc4 -> byVal zp (cmp \cpu -> cpu!regY) cpu
		0xcc -> byVal abs (cmp \cpu -> cpu!regY) cpu

		0xc6 -> inplace zp dec cpu
		0xd6 -> inplace zpX dec cpu
		0xce -> inplace abs dec cpu
		0xde -> inplace absX dec cpu
		0xca -> implied (\cpu -> cpu!regX) (\v cpu -> {cpu & regX=v}) dec cpu
		0x88 -> implied (\cpu -> cpu!regY) (\v cpu -> {cpu & regY=v}) dec cpu

		0x49 -> imm eor cpu
		0x45 -> byVal zp eor cpu
		0x55 -> byVal zpX eor cpu
		0x4d -> byVal abs eor cpu
		0x5d -> byVal absX eor cpu
		0x59 -> byVal absY eor cpu
		0x41 -> byVal xInd eor cpu
		0x51 -> byVal indY eor cpu

		0x18 -> setFlag carry False cpu
		0x38 -> setFlag carry True cpu
		0x58 -> setFlag interruptEnable False cpu
		0x78 -> setFlag interruptEnable True cpu
		0xb8 -> setFlag overflow False cpu
		0xd8 -> setFlag decimal False cpu
		0xf8 -> setFlag decimal True cpu

		0xe6 -> inplace zp inc cpu
		0xf6 -> inplace zpX inc cpu
		0xee -> inplace abs inc cpu
		0xfe -> inplace absX inc cpu
		0xe8 -> implied (\cpu -> cpu!regX) (\v cpu -> {cpu & regX=v}) inc cpu
		0xc8 -> implied (\cpu -> cpu!regY) (\v cpu -> {cpu & regY=v}) inc cpu

		0x4c
			# (a,cpu) = fetchAddr cpu
			-> {cpu & pc=a}
		0x6c
			# (a,cpu) = fetchAddr cpu
			  (v,cpu) = readMemAddr a cpu
			-> {cpu & pc=v}

		0x20
			# (a,cpu) = fetchAddr cpu
			-> jsr a cpu

		0xa9 -> imm (load \v cpu -> {cpu & regA=v}) cpu
		0xa5 -> byVal zp (load \v cpu -> {cpu & regA=v}) cpu
		0xb5 -> byVal zpX (load \v cpu -> {cpu & regA=v}) cpu
		0xad -> byVal abs (load \v cpu -> {cpu & regA=v}) cpu
		0xbd -> byVal absX (load \v cpu -> {cpu & regA=v}) cpu
		0xb9 -> byVal absY (load \v cpu -> {cpu & regA=v}) cpu
		0xa1 -> byVal xInd (load \v cpu -> {cpu & regA=v}) cpu
		0xb1 -> byVal indY (load \v cpu -> {cpu & regA=v}) cpu

		0xa2 -> imm (load \v cpu -> {cpu & regX=v}) cpu
		0xa6 -> byVal zp (load \v cpu -> {cpu & regX=v}) cpu
		0xb6 -> byVal zpY (load \v cpu -> {cpu & regX=v}) cpu
		0xae -> byVal abs (load \v cpu -> {cpu & regX=v}) cpu
		0xbe -> byVal absY (load \v cpu -> {cpu & regX=v}) cpu

		0xa0 -> imm (load \v cpu -> {cpu & regY=v}) cpu
		0xa4 -> byVal zp (load \v cpu -> {cpu & regY=v}) cpu
		0xb4 -> byVal zpY (load \v cpu -> {cpu & regY=v}) cpu
		0xac -> byVal abs (load \v cpu -> {cpu & regY=v}) cpu
		0xbc -> byVal absY (load \v cpu -> {cpu & regY=v}) cpu

		0x4a -> implied (\cpu -> cpu!regA) (\v cpu -> {cpu & regA=v}) lsr cpu
		0x46 -> inplace zp lsr cpu
		0x56 -> inplace zpX lsr cpu
		0x4e -> inplace abs lsr cpu
		0x5e -> inplace absX lsr cpu

		0xea -> cpu // NOP

		0x09 -> imm ora cpu
		0x05 -> byVal zp ora cpu
		0x15 -> byVal zpX ora cpu
		0x0d -> byVal abs ora cpu
		0x1d -> byVal absX ora cpu
		0x19 -> byVal absY ora cpu
		0x01 -> byVal xInd ora cpu
		0x11 -> byVal indY ora cpu

		0xaa -> transfer (\cpu -> cpu!regA) (\v cpu -> {cpu & regX=v}) cpu
		0x8a -> transfer (\cpu -> cpu!regX) (\v cpu -> {cpu & regA=v}) cpu
		0xa8 -> transfer (\cpu -> cpu!regA) (\v cpu -> {cpu & regY=v}) cpu
		0x98 -> transfer (\cpu -> cpu!regY) (\v cpu -> {cpu & regA=v}) cpu

		0x2a -> implied (\cpu -> cpu!regA) (\v cpu -> {cpu & regA=v}) rol cpu
		0x26 -> inplace zp rol cpu
		0x36 -> inplace zpX rol cpu
		0x2e -> inplace abs rol cpu
		0x3e -> inplace absX rol cpu

		0x6a -> implied (\cpu -> cpu!regA) (\v cpu -> {cpu & regA=v}) ror cpu
		0x66 -> inplace zp ror cpu
		0x76 -> inplace zpX ror cpu
		0x6e -> inplace abs ror cpu
		0x7e -> inplace absX ror cpu

		// 0x40 -> rti // TODO
		0x60 -> rts cpu

		0xe9 -> imm sbc cpu
		0xe5 -> byVal zp sbc cpu
		0xf5 -> byVal zpX sbc cpu
		0xed -> byVal abs sbc cpu
		0xfd -> byVal absX sbc cpu
		0xf9 -> byVal absY sbc cpu
		0xe1 -> byVal xInd sbc cpu
		0xf1 -> byVal indY sbc cpu

		0x85 -> byRef zp (store \cpu -> cpu!regA) cpu
		0x95 -> byRef zpX (store \cpu -> cpu!regA) cpu
		0x8d -> byRef abs (store \cpu -> cpu!regA) cpu
		0x9d -> byRef absX (store \cpu -> cpu!regA) cpu
		0x99 -> byRef absY (store \cpu -> cpu!regA) cpu
		0x81 -> byRef xInd (store \cpu -> cpu!regA) cpu
		0x91 -> byRef indY (store \cpu -> cpu!regA) cpu

		0x86 -> byRef zp (store \cpu -> cpu!regX) cpu
		0x96 -> byRef zpY (store \cpu -> cpu!regX) cpu
		0x8e -> byRef abs (store \cpu -> cpu!regX) cpu

		0x84 -> byRef zp (store \cpu -> cpu!regY) cpu
		0x94 -> byRef zpY (store \cpu -> cpu!regY) cpu
		0x8c -> byRef abs (store \cpu -> cpu!regY) cpu

		0x9a -> transfer (\cpu -> cpu!regX) (\v cpu -> {cpu & sp=v}) cpu
		0xba -> transfer (\cpu -> cpu!sp) (\v cpu -> {cpu & regX=v}) cpu
		0x48
			# (a,cpu) = cpu!regA
			-> push a cpu
		0x68
			# (a,cpu) = pop cpu
			-> {cpu & regA=a}
		0x08
			# (s,cpu) = cpu!status
			-> push (toByte (toInt s bitor 0x10)) cpu
		0x28
			# (s,cpu) = pop cpu
			-> {cpu & status=s}

		op -> abort ("unknown instruction " +++ toString op +++ " at address " +++ toString (toInt cpu.pc-1))
where
	imm op cpu
		# (v,cpu) = fetch cpu
		= op v cpu
	byVal addressing op cpu
		# (a,cpu) = addressing cpu
		  (v,cpu) = readMem a cpu
		= op v cpu
	byRef addressing op cpu
		# (a,cpu) = addressing cpu
		= op a cpu
	inplace addressing op cpu
		# (a,cpu) = addressing cpu
		  (v,cpu) = readMem a cpu
		  (v,cpu) = op v cpu
		  cpu = writeMem a v cpu
		= cpu
	implied getReg setReg op cpu
		# (v,cpu) = getReg cpu
		  (v,cpu) = op v cpu
		  cpu = setReg v cpu
		= cpu

	zp` offset cpu
		# (z,cpu) = fetch cpu
		= (toAddr (z + offset), cpu)
	abs` offset cpu
		# (base,cpu) = fetchAddr cpu
		= (base + toAddr offset, cpu)

	zp cpu = zp` (Byte 0) cpu
	zpX cpu=:{regX} = zp` regX cpu
	zpY cpu=:{regY} = zp` regY cpu
	abs cpu = abs` (Byte 0) cpu
	absX cpu=:{regX} = abs` regX cpu
	absY cpu=:{regY} = abs` regY cpu

	xInd cpu
		# (z,cpu) = fetch cpu
		  (offset,cpu) = cpu!regX
		  ref = toAddr (z + offset)
		= readMemAddr ref cpu

	indY cpu
		# (z,cpu) = fetch cpu
		  (offset,cpu) = cpu!regY
		  (base,cpu) = readMemAddr (toAddr z) cpu
		= (base + toAddr offset, cpu)

	updateFlags result cpu
		# cpu = setFlag zero (toInt result bitand 0xff == 0) cpu
		  cpu = setFlag negative (toInt result bitand 0x80 <> 0) cpu
		= cpu

	alu f v cpu
		# v = f v
		# cpu = updateFlags v cpu
		= (v, cpu)

	signed :: !(Byte Byte Bool -> Addr) !Byte !Byte !CPU -> (!Byte, !CPU)
	signed f v1 v2 cpu
		# (c0,cpu) = getFlag carry cpu
		# result = f v1 v2 c0
		# cpu = if ((toInt result bitand 0x80) <> (toInt v1 bitand toInt v2 bitand 0x80))
			(setFlag overflow True cpu)
			cpu
		  cpu = setFlag carry (toInt result >= 0x100) cpu
		  cpu = updateFlags result cpu
		= (toByte result, cpu)

	sub = signed \v1 v2 c0 -> toAddr v1 - toAddr v2 - Addr (if c0 0 1) // TODO: BCD

	cmp getReg v cpu
		# cpu = setFlag carry True cpu
		  (a,cpu) = getReg cpu
		  (_,cpu) = sub a v cpu
		= cpu

	adc v cpu=:{regA}
		# (result,cpu) = signed (\v1 v2 c0 -> toAddr v1 + toAddr v2 + Addr (if c0 1 0)) regA v cpu
		  cpu & regA = result
		= cpu

	sbc v cpu=:{regA}
		# (a,cpu) = sub regA v cpu
		  cpu & regA = a
		= cpu

	and v cpu=:{regA}
		# (v,cpu) = alu (\x -> toByte (toInt x bitand toInt v)) regA cpu
		  cpu & regA = v
		= cpu
	eor v cpu=:{regA}
		# (v,cpu) = alu (\x -> toByte (toInt x bitxor toInt v)) regA cpu
		  cpu & regA = v
		= cpu
	ora v cpu=:{regA}
		# (v,cpu) = alu (\x -> toByte (toInt x bitor toInt v)) regA cpu
		  cpu & regA = v
		= cpu

	dec = alu (\x -> x - Byte 1)
	inc = alu (\x -> x + Byte 1)

	bit v cpu=:{regA}
		# cpu = setFlag zero (toInt regA bitand toInt v == 0) cpu
		  cpu = setFlag negative (toInt v bitand 0x80 <> 0) cpu
		  cpu = setFlag overflow (toInt v bitand 0x40 <> 0) cpu
		= cpu

	load setReg v cpu
		# cpu = setFlag zero (toInt v == 0) cpu
		  cpu = setFlag negative (toInt v bitand 0x80 <> 0) cpu
		  cpu = setReg v cpu
		= cpu

	store getReg addr cpu
		# (v,cpu) = getReg cpu
		  cpu = writeMem addr v cpu
		= cpu

	jsr addr cpu=:{pc}
		# cpu = pushAddr (pc - Addr 1) cpu
		  cpu & pc = addr
		= cpu

	transfer getFrom setTo cpu
		# (v,cpu) = getFrom cpu
		  cpu = updateFlags v cpu
		  cpu = setTo v cpu
		= cpu

	shiftRot f v cpu
		# (c,cpu) = getFlag carry cpu
		  (c,v) = f c v
		  cpu = setFlag carry c cpu
		  cpu = updateFlags v cpu
		= (v, cpu)

	asl = shiftRot \c v -> (toInt v bitand 0x80 <> 0, toByte (toInt v << 1))
	lsr = shiftRot \c v -> (toInt v bitand 0x00 <> 0, toByte (toInt v >> 1))
	rol = shiftRot \c v -> (toInt v bitand 0x80 <> 0, toByte ((toInt v << 1) bitor (if c 0x01 0x00)))
	ror = shiftRot \c v -> (toInt v bitand 0x00 <> 0, toByte ((toInt v >> 1) bitor (if c 0x80 0x00)))

	br :: !Flag !Bool !CPU -> CPU
	br flag target cpu
		# (offset,cpu) = fetch cpu
		# (b,cpu) = getFlag flag cpu
		| b <> target = cpu
		# (pc,cpu) = cpu!pc
		# cpu & pc = pc + toAddr offset - Addr (if (toInt offset < 0x80) 0 0x100)
		= cpu
