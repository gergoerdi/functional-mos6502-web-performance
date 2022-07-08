let implementations = {};

function printOut(s) {
    console.log(s);
}

async function measure(label, act) {
    const buf = files["data/program.dat"].slice();

    const before = +new Date();
    const cnt = await act(buf);
    const after = +new Date();
    if (cnt != 4142) throw { label: label, cnt: cnt };

    const time = after - before;
    // printOut(label + ": " + cnt + " cycles done in " + time + "ms");
    return time;
};

async function measureAll() {
    const numRuns = 100;
    const numWarmup = 100;

    for (const [label, act] of Object.entries(implementations)) {
        for (let i = 0; i < numWarmup; ++i) {
            await measure(label, act);
        }

        let times = [];
        for (let i = 0; i < numRuns; ++i) {
            if (i % 20 == 0) {
                printOut("Running " + label + "...");
            }
            times.push(await measure(label, act));
        }

        let minTime = null, sumTime = 0, maxTime = null;
        for (const time of times) {
            minTime = !minTime || time < minTime ? time : minTime;
            maxTime = !maxTime || time > maxTime ? time : maxTime;
            sumTime += time;
        }

        const avgTime = sumTime / numRuns;

        printOut(label + ":" +
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

    {
        const mod = await import("../implementations/rescript/src/Main.bs.js");
        implementations["ReScript"] = async buf => mod.main(buf);
    }
}

setup().then({});
