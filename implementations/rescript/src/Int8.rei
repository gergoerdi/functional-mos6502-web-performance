type int8;
let string_of_int8 : int8 => string;
let int8_of_int : int => int8;
let int_of_int8 : int8 => int;

let (+) : (int8, int8) => int8;
let (-) : (int8, int8) => int8;
let bor : (int8, int8) => int8;
let band : (int8, int8) => int8;
let bxor : (int8, int8) => int8;
let bshl : (int8, int) => int8;
let bshr : (int8, int) => int8;
let bnot : int8 => int8;
  
