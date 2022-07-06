type int8 = int;

let string_of_int8 = string_of_int
let int8_of_int = x => x land 0xff
let int_of_int8 = x => x
  
let (+) = (x, y) => int8_of_int(Pervasives.(x + y))
let (-) = (x, y) => int8_of_int(Pervasives.(x - y))
let band = (x, y) => x land y
let bor = (x, y) => x lor y
let bxor = (x, y) => x lxor y
let bnot = x => lnot(x)

let bshl = (x, n) => int8_of_int(x lsl n)
let bshr = (x, n) => int8_of_int(x lsr n)
