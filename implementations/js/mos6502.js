export function run(loadFile) {
    return function() {
        let mem = new DataView(loadFile("data/program.dat")());
        let cpu = newCPU(0x438b);

        let r = {
            cpu: cpu,
            readMem: addr => mem.getUint8(addr),
            writeMem: (addr, v) => mem.setUint8(addr, v)
        };

        for (var cnt = 0; r.cpu.pc != 0x640b; ++cnt)
            step(r);
        return cnt;
    };
};
    
function newCPU(pc) {
    return {
        regA: 0x00,
        regX: 0x00,
        regY: 0x00,
        status: 0x00,
        sp: 0xff,
        pc: pc
    };
}
    
function fetch(r) {
    let addr = r.cpu.pc;
    r.cpu.pc = (addr + 1) & 0xffff;
    return r.readMem(addr);
}

let toAddr = (lo, hi) => (hi << 8 | lo);

let fetchAddr = r => toAddr(fetch(r), fetch(r));

let readMemAddr = (r, addr) => toAddr(r.readMem(addr), r.readMem((addr + 1) & 0xffff) );

let push = (r,v) => {
    let ptr = r.cpu.sp;
    r.writeMem((0x100 + ptr) & 0xffff, v);
    r.cpu.sp = (ptr - 1) & 0xff;
};

let pushAddr = (r,addr) => {
    let hi = addr >> 8;
    let lo = addr & 0xff;
    push(r,hi);
    push(r,lo);
};

let pop = r => {
    let ptr = r.cpu.sp;
    let v = r.readMem(0x100 + ((ptr + 1) & 0xff));
    r.cpu.sp = (ptr + 1) & 0xff;
    return v;
};

let popAddr = r => toAddr(pop(r), pop(r));

var cnt = 0;


let getFlag = (r, flag) => (r.cpu.status & flag) != 0;

function setFlag(r, flag, b) {
    if (b) {
        r.cpu.status |= flag;
    } else {
        r.cpu.status &= (~flag & 0xff);
    }
}

let statusFlag = (i) => 0x01 << i;

let carry = statusFlag(0);
let zero = statusFlag(1);
let interruptEnable = statusFlag(2);
let decimal = statusFlag(3);
let overflow = statusFlag(6);
let negative = statusFlag(7);

function step(r) {
    let imm = op => op(fetch(r));
    let byVal = (addressing,op) => op(r.readMem(addressing()));
    let byRef = (addressing,op) => op(addressing());
    let inplace = (addressing,op) => byRef(addressing, addr => r.writeMem(addr, op(r.readMem(addr))));
    let implied = (reg,op) => r.cpu[reg] = op(r.cpu[reg]) & 0xff;

    let zp_ = (offset) => () => (fetch(r) + offset) & 0xffff;
    let abs_ = (offset) => () => fetchAddr(r) + offset;

    let zp = zp_(0);
    let abs = abs_(0);
    let absX = abs_(r.cpu.regX);
    let absY = abs_(r.cpu.regY);

    let xInd = () => {
        let z = fetch(r);
        let offset = r.cpu.regX;
        let ref = (z + offset) & 0xffff;
        return readMemAddr(r, ref);
    };

    let indY = () => {
        let z = fetch(r);
        let offset = r.cpu.regY;
        let base = readMemAddr(r, z);
        return (base + offset) & 0xffff;
    };

    let signed = (f, v1, v2) => {
        let c0 = getFlag(r,carry);
        let result = f(v1, v2, c0);
        // TODO: BCD

        if ((result & 0x80) != (v1 & v2) & 0x80) {
            setFlag(r,overflow, true);
        }
        setFlag(r, carry,result >= 0x100);
        setFlag(r, zero, result & 0xff == 0x00);
        setFlag(r, negative, result & 0x80);
        return (result & 0xff);
    };

    let adc = v => {
        let a = r.cpu.regA;
        r.cpu.regA = signed((v1, v2, c0) => v1 + v2 + (c0 ? 1 : 0), a, v);
    };

    let sub = (v1, v2) => {
        let c0 = getFlag(r,carry);
        let extended = (v1 - v2 - (c0 ? 0 : 1)) & 0xffff; // TODO: BCD
        if ((extended & 0x80) != ((v1 & 0x80) & (v2 & 0x80))) {
            setFlag(r,overflow, true);
        }
        setFlag(r,carry, extended >= 0x100);
        setFlag(r,zero, (extended & 0xff) == 0x00);
        setFlag(r,negative, (extended & 0x80) != 0x00);
        
        return (extended & 0xff);
    };
    
    let cmp = a => v => {
        setFlag(r,carry,true);
        sub(a,v);
    };
    
    let sbc = v => {
        r.cpu.regA = sub(r.cpu.regA, v);
    };
    
    let alu = f => a => {
        let result = f(a);

        setFlag(r, zero, (result & 0xff) == 0x00);
        setFlag(r, negative, (result & 0x80) != 0x00);
        return result;
    };

    let and = v => r.cpu.regA = alu(x => x & v)(r.cpu.regA);
    let eor = v => r.cpu.regA = alu(x => x ^ v)(r.cpu.regA);
    let ora = v => r.cpu.regA = alu(x => x | v)(r.cpu.regA);

    let shiftRot = f => v => {
        let c = getFlag(r,carry);
        let cv = f(c,v);
        setFlag(r, carry, cv.c);
        setFlag(r, zero, (cv.v & 0xff) == 0);
        setFlag(r, negative, (cv.v & 0x80) != 0x00);
        return cv.v;       
    };

    let asl = shiftRot((c,v) => ({ c: v & 0x80, v: ((v << 1) & 0xff) }));
    let lsr = shiftRot((c,v) => ({ c: v & 0x01, v: ((v >> 1) & 0xff) }));
    let rol = shiftRot((c,v) => ({ c: v & 0x80, v: (((v << 1) & 0xff) | (c ? 0x01 : 0x000)) }));
    let ror = shiftRot((c,v) => ({ c: v & 0x01, v: (((v >> 1) & 0xff) | (c ? 0x80 : 0x00)) }));

    let bit = v => {
        let a = r.cpu.regA;
        setFlag(r, zero, (a & v) == 0);
        setFlag(r, negative, (v & 0x80) != 0);
        setFlag(r, overflow, (v & 0x40) != 0);
    };

    let br = (flag, target) => {
        let offset = fetch(r) & 0xff;
        let b = getFlag(r, flag);
        if (b == target) {
            // console.log("Branch taken from " + (r.cpu.pc - 2).toString(16));
            r.cpu.pc =
                offset < 0x80 ? r.cpu.pc + offset :
                r.cpu.pc + offset - 0x100;
        }
    };

    let dec = alu(v => (v - 1) & 0xff);
    let inc = alu(v => (v + 1) & 0xff);
    
    let load = reg => v => {
        setFlag(r, zero, (v & 0xff) == 0x00);
        setFlag(r, negative, (v & 0x80) != 0x00);
        r.cpu[reg] = v;
    };
    let store = reg => addr => {
        // console.log("store", addr, reg, r.cpu, r.cpu[reg]);
        r.writeMem(addr, r.cpu[reg]);
    };
    let jsr = addr => {
        let curr = r.cpu.pc;
        pushAddr(r, (curr - 1) & 0xffff);
        r.cpu.pc = addr;
    };
    let transfer = (from, to) => {
        let v = r.cpu[from];
        r.cpu[to] = alu(() => 0)(0);
    };

    // console.log(r.cpu.pc.toString(16), r.cpu);
    let op = fetch(r);
    switch (op) {
    case 0x69: imm(adc); break;
    case 0x65: byVal(zp,adc); break;
    case 0x75: byVal(zpX,adc); break;
    case 0x6d: byVal(abs, adc); break;
    case 0x7d: byVal(absX, adc); break;
    case 0x79: byVal(absY, adc); break;
    case 0x61: byVal(xInd, adc); break;
    case 0x71: byVal(indY, adc); break;

    case 0x29: imm(and); break;
    case 0x25: byVal(zp,and); break;
    case 0x35: byVal(zpX,and); break;
    case 0x2d: byVal(abs, and); break;
    case 0x3d: byVal(absX, and); break;
    case 0x39: byVal(absY, and); break;
    case 0x21: byVal(xInd, and); break;
    case 0x31: byVal(indY, and); break;

    case 0x0a: implied("regA", asl); break;
    case 0x06: inplace(zp, asl); break;
    case 0x16: inplace(zpX, asl); break;
    case 0x0e: inplace(abs, asl); break;
    case 0x1e: inplace(absX, asl); break;

    case 0x24: byVal(zp, bit); break;
    case 0x2c: byVal(abs, bit); break;

    case 0x10: br(negative, false); break;
    case 0x30: br(negative, true); break;
    case 0x50: br(overflow, false); break;
    case 0x70: br(overflow, true); break;
    case 0x90: br(carry, false); break;
    case 0xb0: br(carry, true); break;
    case 0xd0: br(zero, false); break;
    case 0xf0: br(zero, true); break;

    // case 0x00: brk(); break; // TODO

    case 0xc9: imm(cmp(r.cpu.regA)); break;
    case 0xc5: byVal(zp, cmp(r.cpu.regA)); break;
    case 0xd5: byVal(zpX, cmp(r.cpu.regA)); break;
    case 0xcd: byVal(abs, cmp(r.cpu.regA)); break;
    case 0xdd: byVal(absX, cmp(r.cpu.regA)); break;
    case 0xd9: byVal(absY, cmp(r.cpu.regA)); break;
    case 0xc1: byVal(xInd, cmp(r.cpu.regA)); break;
    case 0xd1: byVal(indY, cmp(r.cpu.regA)); break;

    case 0xe0: imm(cmp(r.cpu.regX)); break;
    case 0xe4: byVal(zp, cmp(r.cpu.regX)); break;
    case 0xec: byVal(abs, cmp(r.cpu.regX)); break;

    case 0xc0: imm(cmp(r.cpu.regY)); break;
    case 0xc4: byVal(zp, cmp(r.cpu.regY)); break;
    case 0xcc: byVal(abs, cmp(r.cpu.regY)); break;

    case 0xc6: inplace(zp, dec); break;
    case 0xd6: inplace(zpX, dec); break;
    case 0xce: inplace(abs, dec); break;
    case 0xde: inplace(absX, dec); break;
    case 0xca: implied("regX", dec); break;
    case 0x88: implied("regY", dec); break;

    case 0x49: imm(eor); break;
    case 0x45: byVal(zp, eor); break;
    case 0x55: byVal(zpX, eor); break;
    case 0x4d: byVal(abs, eor); break;
    case 0x5d: byVal(absX, eor); break;
    case 0x59: byVal(absY, eor); break;
    case 0x41: byVal(xInd, eor); break;
    case 0x51: byVal(indY, eor); break;

    case 0x18: setFlag(r, carry, false); break;
    case 0x38: setFlag(r, carry, true); break;
    case 0x58: setFlag(r, interruptEnable, false); break;
    case 0x78: setFlag(r, interruptEnable, true); break;
    case 0xb8: setFlag(r, overflow, false); break;
    case 0xd8: setFlag(r, decimal, false); break;
    case 0xf8: setFlag(r, decimal, true); break;

    case 0xe6: inplace(zp, inc); break;
    case 0xf6: inplace(zpX, inc); break;
    case 0xee: inplace(abs, inc); break;
    case 0xfe: inplace(absX, inc); break;
    case 0xe8: implied("regX", inc); break;
    case 0xc8: implied("regY", inc); break;
        
    case 0x4c: r.cpu.pc = fetchAddr(r); break;
    case 0x6c: r.cpu.pc = readMemAddr(r, fetchAddr(r)); break;

    case 0x20: jsr(fetchAddr(r)); break;

    case 0xa9: imm(load("regA")); break;
    case 0xa5: byVal(zp, load("regA")); break;
    case 0xb5: byVal(zpX, load("regA")); break;
    case 0xad: byVal(abs, load("regA")); break;
    case 0xbd: byVal(absX, load("regA")); break;
    case 0xb9: byVal(absY, load("regA")); break;
    case 0xa1: byVal(xInd, load("regA")); break;
    case 0xb1: byVal(indY, load("regA")); break;
        
    case 0xa2: imm(load("regX")); break;
    case 0xa6: byVal(zp, load("regX")); break;
    case 0xb6: byVal(zpY, load("regX")); break;
    case 0xae: byVal(abs, load("regX")); break;
    case 0xbe: byVal(absY, load("regX")); break;

    case 0xa0: imm(load("regY")); break;
    case 0xa4: byVal(zp, load("regY")); break;
    case 0xb4: byVal(zpY, load("regY")); break;
    case 0xac: byVal(abs, load("regY")); break;
    case 0xbc: byVal(absY, load("regY")); break;

    case 0x4a: implied("regA", lsr); break;
    case 0x46: inplace(zp, lsr); break;
    case 0x56: inplace(zpX, lsr); break;
    case 0x4e: inplace(abs, lsr); break;
    case 0x5e: inplace(absX, lsr); break;

    case 0xea: break; // NOP

    case 0x09: imm(ora); break;
    case 0x05: byVal(zp, ora); break;
    case 0x15: byVal(zpX, ora); break;
    case 0x0d: byVal(abs, ora); break;
    case 0x1d: byVal(absX, ora); break;
    case 0x19: byVal(absY, ora); break;
    case 0x01: byVal(xInd, ora); break;
    case 0x11: byVal(indY, ora); break;

    case 0xaa: transfer("regA", "regX"); break;
    case 0x8a: transfer("regX", "regA"); break;
    case 0xa8: transfer("regA", "regY"); break;
    case 0x98: transfer("regY", "regA"); break;

    case 0x2a: implied("regA", rol); break;
    case 0x26: inplace(zp, rol); break;
    case 0x36: inplace(zpX, rol); break;
    case 0x2e: inplace(abs, rol); break;
    case 0x3e: inplace(absX, rol); break;

    case 0x6a: implied("regA", ror); break;
    case 0x66: inplace(zp, ror); break;
    case 0x76: inplace(zpX, ror); break;
    case 0x6e: inplace(abs, ror); break;
    case 0x7e: inplace(absX, ror); break;

    // case 0x40: rti(); break; // TODO
    case 0x60: rts(r); break;

    case 0xe9: imm(sbc); break;
    case 0xe5: byVal(zp,sbc); break;
    case 0xf5: byVal(zpX,sbc); break;
    case 0xed: byVal(abs, sbc); break;
    case 0xfd: byVal(absX, sbc); break;
    case 0xf9: byVal(absY, sbc); break;
    case 0xe1: byVal(xInd, sbc); break;
    case 0xf1: byVal(indY, sbc); break;

    case 0x85: byRef(zp, store("regA")); break;
    case 0x95: byRef(zpX, store("regA")); break;
    case 0x8d: byRef(abs, store("regA")); break;
    case 0x9d: byRef(absX, store("regA")); break;
    case 0x99: byRef(absY, store("regA")); break;
    case 0x81: byRef(xInd, store("regA")); break;
    case 0x91: byRef(indY, store("regA")); break;

    case 0x86: byRef(zp, store("regX")); break;
    case 0x96: byRef(zpY, store("regX")); break;
    case 0x8e: byRef(abs, store("regX")); break;

    case 0x84: byRef(zp, store("regY")); break;
    case 0x94: byRef(zpY, store("regY")); break;
    case 0x8c: byRef(abs, store("regY")); break;

    case 0x9a: transfer("regX", "sp"); break;
    case 0xba: transfer("sp", "regX"); break;
    case 0x48: push(r, r.cpu.regA); break;
    case 0x68: r.cpu.regA = pop(r); break;
    case 0x08: push(r, r.cpu.status | 0x10); break;
    case 0x28: r.cpu.status = pop(r); break;

    default: throw(op.toString(16));
    };
};

let rts = r => {
    let addr = popAddr(r);
    r.cpu.pc = (addr + 1) & 0xffff;
};
