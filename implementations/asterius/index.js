import * as rts from "./rts.mjs";
import module from "./Driver.wasm.mjs";
import req from "./Driver.req.mjs";

export async function setup() {
    const m = await module;
    return async arg => {
        const i = await rts.newAsteriusInstance(Object.assign(req, { module: m }));
        const result = await i.exports.run(arg)
        return result;
    };
}
