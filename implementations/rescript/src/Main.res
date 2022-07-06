let main = buf => {

  let mem = Js.TypedArray2.Uint8Array.fromBuffer(buf)

  module Machine : MOS6502.Machine = {
    let readMem = addr => Int8.int8_of_int(Js.TypedArray2.Uint8Array.unsafe_get(mem, Int16.int_of_int16(addr)))
    let writeMem = (addr, v) => Js.TypedArray2.Uint8Array.unsafe_set(mem, Int16.int_of_int16(addr), Int8.int_of_int8(v))
  }

  module Init: MOS6502.Initialize = {
    let pc0 = Int16.int16_of_int(0x438b)
  }

  module MOS6502 = MOS6502.CPU(Machine, Init)

  let rec run = (cnt : int) => {
    let pc = MOS6502.getPC()
    if pc == Int16.int16_of_int(0x640b) { cnt } else {
      MOS6502.step()
      run(cnt + 1)
    }
  }

  run(0)
}
