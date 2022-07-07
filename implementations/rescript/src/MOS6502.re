open Int8

type byte = int8
type addr = Int16.int16

module type Initialize = {
  let pc0 : addr
}

module type Machine = {
  let readMem : addr => byte
  let writeMem : (addr, byte) => unit
}
  
module CPU = (Machine: Machine, Init: Initialize) => {
  let regA = ref(int8_of_int(0))
  let regX = ref(int8_of_int(0))
  let regY = ref(int8_of_int(0))
  let status = ref(int8_of_int(0))
  let sp = ref(int8_of_int(0))
  let pc = ref(Init.pc0)

  let getPC = () => pc^
  
  open Machine
  
  let fetch = () => {
    let x = readMem(pc^)  
    pc := Int16.(pc^ + int16_of_int(1))
    x  
  };

  let toAddr = (lo, hi) => Int16.(int16_of_int(Pervasives.(int_of_int8(hi) lsl 8)) + int16_of_int(int_of_int8(lo)))

  let fetchAddr = () => toAddr(fetch(), fetch())
  let readMemAddr = addr => toAddr(readMem(addr), readMem(Int16.(addr + int16_of_int(1))))

  let push(v) = {
    writeMem(toAddr(sp^, int8_of_int(0x01)), v)
    sp := Int8.(sp^ - int8_of_int(1))
  }

  let pushAddr(addr) = {
    let lo = int8_of_int(Int16.int_of_int16(addr))
    let hi = int8_of_int(Pervasives.(Int16.int_of_int16(addr) lsr 8))
    push(hi)
    push(lo)
  }

  let pop = () => {
    sp := Int8.(sp^ + int8_of_int(1))
    readMem(toAddr(sp^, int8_of_int(0x01)))
  }

  let popAddr = () => toAddr(pop(), pop())
  
  let getFlag = fl => {
    band(status^, fl) != int8_of_int(0x00)
  }
  
  let setFlag = (fl, b) => {
    status := if (b) { bor(status^, fl) } else { band(status^, bnot(fl)) }
  }
  
  let carry = int8_of_int(0x01)
  let zero = int8_of_int(0x02)
  let interruptEnable = int8_of_int(0x04)
  let decimal = int8_of_int(0x08)
  let overflow = int8_of_int(0x40)
  let negative = int8_of_int(0x80)
  
  let rts = () => {
    pc := Int16.(popAddr() + int16_of_int(1))
  }

  let updateFlags = result => {
    setFlag(zero, band(result, int8_of_int(0xff)) == int8_of_int(0))
    setFlag(negative, band(result, int8_of_int(0x80)) == int8_of_int(0))
    result
  }

  let transfer = (src,dest) => dest := updateFlags(src^)

  exception UnkownOpcode(byte);
  
  let step = () => {
    let byVal = (addressing, op) => op(readMem(addressing()))
    let byRef = (addressing, op) => op(addressing())
    let inplace = (addressing, op) => {
      let addr = addressing()
      writeMem(addr, op(readMem(addr)))
    }
    let implied = (reg, op) => reg := op(reg^)
    let imm = op => op(fetch())

    let store = reg => addr => writeMem(addr, reg^)
    let load = reg => v => {
      reg := updateFlags(v)
    }

    let zp_ = offset => () => {
      let z = fetch()
      Int16.int16_of_int(int_of_int8(z + offset))
    }
    let abs_ = offset => () => {
      let base = fetchAddr()
      Int16.(base + int16_of_int(int_of_int8(offset)))
    }
    
    let zp = zp_(int8_of_int(0))
    let zpX = zp_(regX^)
    let zpY = zp_(regX^)
    let abs = abs_(int8_of_int(0))
    let absX = abs_(regX^)
    let absY = abs_(regY^)

    let xInd = () => {
      let z = fetch()
      readMemAddr(Int16.int16_of_int(int_of_int8(z + regX^)))
    }

    let indY = () => {
      let z = fetch()
      let offset = regY^
      let base = readMemAddr(Int16.int16_of_int(int_of_int8(z)))
      Int16.(base + int16_of_int(int_of_int8(offset)))
    }
    
    let alu = f => v => updateFlags(f(v))
    
    let signed = (f : (int, int, bool) => int) => (v1, v2) => {
      let v1' = int_of_int8(v1)
      let v2' = int_of_int8(v2)
      
      let c0 = getFlag(carry)
      let result = f(v1', v2', c0)
      if (Pervasives.((result land 0x80) != (v1' land v2') land 0x80)) { setFlag(overflow, true) }
      setFlag(carry, result >= 0x100 || result < 0)
      updateFlags(int8_of_int(result))
    }

    let sub = signed(fun (v1, v2, c0) => Pervasives.(v1 - v2 - if (c0) 0 else 1))

    let cmp = reg => v => {
      setFlag(carry, true)
      let _ = sub(reg^, v)
    }

    let sbc = v => regA := sub(regA^, v)

    let bit = v => {
      let a = regA^
      setFlag(zero, band(a, v) == int8_of_int(0))
      setFlag(negative, band(v, int8_of_int(0x80)) != int8_of_int(0))
      setFlag(overflow, band(v, int8_of_int(0x40)) != int8_of_int(0))
    }

    let jsr = addr => {
      pushAddr(Int16.(pc^ - int16_of_int(1)))
      pc := addr;
    }

    let br = (flag, target) => {
      let offset = Int16.int16_of_int(int_of_int8(fetch()))
      if (getFlag(flag) == target) {
        pc := Int16.(pc^ + offset - int16_of_int(if (offset < int16_of_int(0x80)) 0 else 0x100))
      }
    }
      
    let adc = v => {
      regA := signed(fun(v1, v2, c0) => Pervasives.(v1 + v2 + if (c0) { 1 } else { 0 }))(regA^, v)
    }

    let shiftRot = f => v => {
      let c = getFlag(carry)
      let (c', v') = f(c, v)
      setFlag(carry, c')
      updateFlags(v')
    }

    let asl  = shiftRot(fun (_c, v) => (band(v, int8_of_int(0x80)) != int8_of_int(0x00), bshl(v, 1)))
    let lsr_ = shiftRot(fun (_c, v) => (band(v, int8_of_int(0x01)) != int8_of_int(0x00), bshr(v, 1)))
    let rol  = shiftRot(fun (c, v) => (band(v, int8_of_int(0x80)) != int8_of_int(0x00), bor(bshl(v, 1), int8_of_int(if (c) 0x01 else 0x00))))
    let ror  = shiftRot(fun (c, v) => (band(v, int8_of_int(0x01)) != int8_of_int(0x00), bor(bshr(v, 1), int8_of_int(if (c) 0x80 else 0x00))))

    let and_ = v => regA := alu(fun (a) => band(a, v))(regA^)
    let eor  = v => regA := alu(fun (a) => bxor(a, v))(regA^)
    let ora  = v => regA := alu(fun (a) => bor(a, v))(regA^)

    let dec = alu(fun (v) => v - int8_of_int(1))
    let inc = alu(fun (v) => v + int8_of_int(1))

    let _ = switch(int_of_int8(fetch())) { // http://www.6502.org/tutorials/6502opcodes.html
      | 0x69 => imm(adc);
      | 0x65 => byVal(zp, adc)
      | 0x75 => byVal(zpX, adc);
      | 0x6d => byVal(abs, adc);
      | 0x7d => byVal(absX, adc);
      | 0x79 => byVal(absY, adc);
      | 0x61 => byVal(xInd, adc);
      | 0x71 => byVal(indY, adc);

      | 0x29 => imm(and_);
      | 0x25 => byVal(zp, and_);
      | 0x35 => byVal(zpX, and_);
      | 0x2d => byVal(abs, and_);
      | 0x3d => byVal(absX, and_);
      | 0x39 => byVal(absY, and_);
      | 0x21 => byVal(xInd, and_);
      | 0x31 => byVal(indY, and_);

      | 0x0a => implied(regA, asl);
      | 0x06 => inplace(zp, asl);
      | 0x16 => inplace(zpX, asl);
      | 0x0e => inplace(abs, asl);
      | 0x1e => inplace(absX, asl);

      | 0x24 => byVal(zp, bit);
      | 0x2c => byVal(abs, bit);

      | 0x10 => br(negative, false);
      | 0x30 => br(negative, true);
      | 0x50 => br(overflow, false);
      | 0x70 => br(overflow, true);
      | 0x90 => br(carry, false);
      | 0xb0 => br(carry, true);
      | 0xd0 => br(zero, false);
      | 0xf0 => br(zero, true);

      /* | 0x00 => brk // TODO */

      | 0xc9 => imm(cmp(regA));
      | 0xc5 => byVal(zp, cmp(regA));
      | 0xd5 => byVal(zpX, cmp(regA));
      | 0xcd => byVal(abs, cmp(regA));
      | 0xdd => byVal(absX, cmp(regA));
      | 0xd9 => byVal(absY, cmp(regA));
      | 0xc1 => byVal(xInd, cmp(regA));
      | 0xd1 => byVal(indY, cmp(regA));
             
      | 0xe0 => imm(cmp(regX));
      | 0xe4 => byVal(zp, cmp(regX));
      | 0xec => byVal(abs, cmp(regX));
             
      | 0xc0 => imm(cmp(regY));
      | 0xc4 => byVal(zp, cmp(regY));
      | 0xcc => byVal(abs, cmp(regY));

      | 0xc6 => inplace(zp, dec);
      | 0xd6 => inplace(zpX, dec);
      | 0xce => inplace(abs, dec);
      | 0xde => inplace(absX, dec);
      | 0xca => implied(regX, dec);
      | 0x88 => implied(regY, dec);

      | 0x49 => imm(eor);
      | 0x45 => byVal(zp, eor);
      | 0x55 => byVal(zpX, eor);
      | 0x4d => byVal(abs, eor);
      | 0x5d => byVal(absX, eor);
      | 0x59 => byVal(absY, eor);
      | 0x41 => byVal(xInd, eor);
      | 0x51 => byVal(indY, eor);

      | 0x18 => setFlag(carry, false);
      | 0x38 => setFlag(carry, true);
      | 0x58 => setFlag(interruptEnable, false);
      | 0x78 => setFlag(interruptEnable, true);
      | 0xb8 => setFlag(overflow, false);
      | 0xd8 => setFlag(decimal, false);
      | 0xf8 => setFlag(decimal, true);

      | 0xe6 => inplace(zp, inc);
      | 0xf6 => inplace(zpX, inc);
      | 0xee => inplace(abs, inc);
      | 0xfe => inplace(absX, inc);
      | 0xe8 => implied(regX, inc);
      | 0xc8 => implied(regY, inc);

      | 0x4c => pc := fetchAddr();
      | 0x6c => pc := readMemAddr(fetchAddr());

      | 0x20 => jsr(fetchAddr());

      | 0xa9 => imm(load(regA))
      | 0xa5 => byVal(zp, load(regA));
      | 0xb5 => byVal(zpX, load(regA));
      | 0xad => byVal(abs, load(regA));
      | 0xbd => byVal(absX, load(regA));
      | 0xb9 => byVal(absY, load(regA));
      | 0xa1 => byVal(xInd, load(regA));
      | 0xb1 => byVal(indY, load(regA));
             
      | 0xa2 => imm(load(regX));
      | 0xa6 => byVal(zp, load(regX));
      | 0xb6 => byVal(zpY, load(regX));
      | 0xae => byVal(abs, load(regX));
      | 0xbe => byVal(absY, load(regX));
             
      | 0xa0 => imm(load(regY));
      | 0xa4 => byVal(zp, load(regY));
      | 0xb4 => byVal(zpY, load(regY));
      | 0xac => byVal(abs, load(regY));
      | 0xbc => byVal(absY, load(regY));

      | 0x4a => implied(regA, lsr_);
      | 0x46 => inplace(zp, lsr_);
      | 0x56 => inplace(zpX, lsr_);
      | 0x4e => inplace(abs, lsr_);
      | 0x5e => inplace(absX, lsr_);

      | 0xea => () // NOP

      | 0x09 => imm(ora);
      | 0x05 => byVal(zp, ora);
      | 0x15 => byVal(zpX, ora);
      | 0x0d => byVal(abs, ora);
      | 0x1d => byVal(absX, ora);
      | 0x19 => byVal(absY, ora);
      | 0x01 => byVal(xInd, ora);
      | 0x11 => byVal(indY, ora);

      | 0xaa => transfer(regA, regX);
      | 0x8a => transfer(regX, regA);
      | 0xa8 => transfer(regA, regY);
      | 0x98 => transfer(regY, regA);

      | 0x2a => implied(regA, rol);
      | 0x26 => inplace(zp, rol);
      | 0x36 => inplace(zpX, rol);
      | 0x2e => inplace(abs, rol);
      | 0x3e => inplace(absX, rol);
                      
      | 0x6a => implied(regA, ror);
      | 0x66 => inplace(zp, ror);
      | 0x76 => inplace(zpX, ror);
      | 0x6e => inplace(abs, ror);
      | 0x7e => inplace(absX, ror);
      
      /* | 0x40 => rti(); // TODO */
      | 0x60 => rts();

      | 0xe9 => imm(sbc);
      | 0xe5 => byVal(zp, sbc);
      | 0xf5 => byVal(zpX, sbc);
      | 0xed => byVal(abs, sbc);
      | 0xfd => byVal(absX, sbc);
      | 0xf9 => byVal(absY, sbc);
      | 0xe1 => byVal(xInd, sbc);
      | 0xf1 => byVal(indY, sbc);

      | 0x85 => byRef(zp, store(regA));
      | 0x95 => byRef(zpX, store(regA));
      | 0x8d => byRef(abs, store(regA));
      | 0x9d => byRef(absX, store(regA));
      | 0x99 => byRef(absY, store(regA));
      | 0x81 => byRef(xInd, store(regA));
      | 0x91 => byRef(indY, store(regA));

      | 0x86 => byRef(zp, store(regX));
      | 0x96 => byRef(zpY, store(regX));
      | 0x8e => byRef(abs, store(regX));

      | 0x84 => byRef(zp, store(regY));
      | 0x94 => byRef(zpY, store(regY));
      | 0x8c => byRef(abs, store(regY));

      | 0x9a => transfer(regX, sp);
      | 0xba => transfer(sp, regX);
      | 0x48 => push(regA^);
      | 0x68 => regA := pop();
      | 0x08 => push(bor(status^, int8_of_int(0x10)));
      | 0x28 => status := pop();
      | op => raise(UnkownOpcode(int8_of_int(op)));
    };
  }
}
