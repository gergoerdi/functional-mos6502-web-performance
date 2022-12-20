function loadFile(s, target) {
    console.log("loadFile: " + s);
    let buf = files[s].slice();

    // return h$wrapBuffer(buf, true, null, null);

    target.len = buf.byteLength;
    let buf8 = new Uint8Array(buf);
    for (var i = 0; i < buf8.length; ++i)
        target.u8[i] = buf8[i];
}
