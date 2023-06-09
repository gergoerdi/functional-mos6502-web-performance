function loadFile(s) {
    console.log("loadFile: " + s);
    const buf = files[s].slice();

    h$ret1 = 0;
    return h$wrapBuffer(buf, true, null, null);
}

function printOut(s) {
    console.log(s);

    const pre = document.getElementById("out");
    pre.appendChild(document.createTextNode(s + "\n"));
}

function numRuns() {
    return 100;
}

var before;

function logStart() {
    console.log("logStart");
    before = +new Date();
}

function logEnd(cnt) {
    const after = +new Date();
    console.log("logEnd");
    const time = after - before;
    const label = "ghcjs"
    printOut(label + ": " + cnt + " cycles done in " + time + "ms");
}
