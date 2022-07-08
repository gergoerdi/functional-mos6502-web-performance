let clean_run=null;

let clean_promise = (() => {
    const interpreter = {
        shared_clean_values: null
    };

    return ABCInterpreter.instantiate({
        bytecode_path: '/implementations/clean/mos6502.pbc',

        fetch: (p) => {
            if (p.indexOf('/js')==0)
                p='/implementations/clean'+p;
            return fetch(p);
        },

        heap_size: 4<<20,
        stack_size: 512<<10,

        encoding: 'utf-8',

        with_js_ffi: true,
        js_ffi_options: {}
    }).then(abc => {
        const asp = abc.interpreter.instance.exports.get_asp();
        const hp = abc.interpreter.instance.exports.get_hp();
        const hp_free = abc.interpreter.instance.exports.get_hp_free();

        const start_node = abc.memory_array[(abc.start>>2)+2];
        abc.memory_array[hp>>2] = start_node;
        abc.memory_array[(hp>>2)+1] = 0;
        abc.interpreter.instance.exports.set_hp(hp+24);
        abc.interpreter.instance.exports.set_hp_free(hp_free-3);

        const index = abc.share_clean_value(hp, interpreter);

        abc.memory_array[asp>>2] = hp;
        abc.memory_array[(asp>>2)+1] = 0;
        abc.interpreter.instance.exports.set_pc(start_node);

        const csp = abc.interpreter.instance.exports.get_csp();
        abc.memory_array[csp/4] = 658*8; // instruction 0; to return
        abc.interpreter.instance.exports.set_csp(csp+8);

        abc.interpreter.instance.exports.interpret();

        abc.interpret(new SharedCleanValue(index), [interpreter, 1]);
    });
})();
