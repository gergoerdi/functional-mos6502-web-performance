async function setup() {
    const mod = await import("./_build/Driver.mjs");
    return await mod.setup();
}

async function main() {
    const run = await setup();

    const before = +new Date();
    const cnt = await run(s => files[s].slice());
    const after = +new Date();

    const label = "ghc-asterius";
    console.log(label + ": " + cnt + " cycles done in " + (after - before) + "ms");
};
