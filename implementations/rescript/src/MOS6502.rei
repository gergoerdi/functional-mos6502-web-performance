open Int8;
open Int16;

type byte = int8;
type addr = int16;

module type Machine = {
  let readMem : addr => byte;
  let writeMem : addr => byte => unit;
};

module type Initialize = {
  let pc0 : addr;
};
  
module CPU: (Machine: Machine, Init: Initialize) => {
  let step: unit => unit;
  let getPC: unit => addr;
};
