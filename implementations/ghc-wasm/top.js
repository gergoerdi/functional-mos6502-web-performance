async function setup() {
    const mod = await import("./driver.js");
    const exports = await mod.setup();

    exports._initialize();
    exports.hs_init();
    return exports;
}

async function main() {
    const exports = await setup();
    const newBuffer = exports.newBuffer;
    const run = exports.run;

    self.onmessage = e => {
        const bytes = new Uint8Array(files["data/program.dat"]);
        const len = bytes.byteLength;
        const buf = newBuffer(len);
        const arr = new Uint8Array(exports.memory.buffer, buf, len);
        for (let i = 0; i < len; ++i) {
            arr[i] = bytes[i];
        }
        
        const before = +new Date();
        const cnt = run(buf, len);
        const after = +new Date();
        
        const label = "ghc-wasm";
        console.log(label + ": " + cnt + " cycles done in " + (after - before) + "ms");
    };
    console.log("All set up, ready to run via postMessage()");

    self.postMessage(0);
};
