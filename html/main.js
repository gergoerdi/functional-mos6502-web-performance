var purs;
var js;

let measure = (label, act) => {
    let before = +new Date();
    let cnt = act();
    let after = +new Date();
    console.log(label + ": " + cnt + " cycles done in " + (after - before) + "ms");
};

import('./implementations/purescript/bundle.js').then(mod => {
    purs = () => {
        measure("PureScript", mod.run(fn => () => files[fn].slice()));
    };
});

let idris2 = () => {
    measure("Idris2", () => idris2_run(fn => w => files[fn].slice()));
}

import('./implementations/js/mos6502.js').then(mod => {
    js = () => {
        measure("Javascript", mod.run(fn => () => files[fn].slice()));
    };
});
