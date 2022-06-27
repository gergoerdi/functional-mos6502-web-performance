let implementations = {};

async function measure(label, act) {
    const buf = files["data/program.dat"].slice();

    const before = +new Date();
    const cnt = await act(buf);
    const after = +new Date();
    if (cnt != 4142) throw { label: label, cnt: cnt };

    const time = after - before;
    // console.log(label + ": " + cnt + " cycles done in " + time + "ms");
    return time;
};

async function measureAll() {
    const numRuns = 20;
    const numWarmup = 5;

    for (const [label, act] of Object.entries(implementations)) {
        for (let i = 0; i < numWarmup; ++i) {
            await measure(label, act);
        }

        let times = [];
        for (let i = 0; i < numRuns; ++i) {
            times.push(await measure(label, act));
        }

        let minTime = null, sumTime = 0, maxTime = null;
        for (const time of times) {
            minTime = !minTime || time < minTime ? time : minTime;
            maxTime = !maxTime || time > maxTime ? time : maxTime;
            sumTime += time;
        }

        const avgTime = sumTime / numRuns;

        console.log(label + ":" +
                    " min: " + minTime + "ms" +
                    " max: " + maxTime + "ms" +
                    " avg: " + avgTime + "ms");
    }
}

async function setup()
{
    {
        const mod = await import("./implementations/js/mos6502.js");
        implementations["JavaScript"] = async buf => mod.run(buf)();
    }

    implementations["Idris2"] = async buf => idris2_run(buf);

    {
        const mod = await import("./implementations/purescript/bundle.js");
        implementations["PureScript"] = async buf => mod.run(buf)();
    }

    {
        const mod = await import("../implementations/asterius/_build/Driver.mjs");
        const run = await mod.setup();
        implementations["GHC-Asterius"] = async buf => await run(buf);
    }
}

setup().then({});
