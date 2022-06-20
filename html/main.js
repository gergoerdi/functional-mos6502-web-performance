let implementations = {};

function measure(label, act) {
    const before = +new Date();
    const cnt = act();
    const after = +new Date();
    if (cnt != 4142) throw { label: label, cnt: cnt };

    const time = after - before;
    // console.log(label + ": " + cnt + " cycles done in " + time + "ms");
    return time;
};

function measureAll() {
    const numRuns = 100;
    const numWarmup = 10;

    for (const [label, act] of Object.entries(implementations)) {
        for (let i = 0; i < numWarmup; ++i) {
            measure(label, act);
        }

        let times = [];
        for (let i = 0; i < numRuns; ++i) {
            times.push(measure(label, act));
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
        const mod = await import("./implementations/purescript/bundle.js");
        implementations["PureScript"] = () => mod.run(fn => () => files[fn].slice())();
    }

    implementations["Idris2"] = () => idris2_run(fn => w => files[fn].slice());

    {
        const mod = await import("./implementations/js/mos6502.js");
        implementations["JavaScript"] = () => mod.run(fn => () => files[fn].slice())();
    }
}

setup().then({});
