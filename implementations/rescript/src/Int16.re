type int16 = int

let string_of_int16 = string_of_int
let int16_of_int = x => x land 0xffff
let int_of_int16 = x => x

let (+) = (x, y) => Pervasives.(x + y) land 0xffff
let (-) = (x, y) => Pervasives.(x - y) land 0xffff
