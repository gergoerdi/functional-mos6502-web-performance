import { WASI } from "./node_modules/@bjorn3/browser_wasi_shim/dist/index.js";

export async function setup() {
    const wasi = new WASI([], [], []);
    const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
    const wasm = await WebAssembly.instantiateStreaming(fetch("./_build/main.wasm"), wasiImportObj);
    wasi.inst = wasm.instance;
    const exports = wasm.instance.exports;
    const memory = exports.memory;

    return exports;
}
