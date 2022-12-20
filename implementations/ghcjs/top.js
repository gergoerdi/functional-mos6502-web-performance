function loadFile(s) {
    console.log("loadFile: " + s);
    let buf = files[s].slice();

    h$ret1 = 0;
    return h$wrapBuffer(buf, true, null, null);
}
