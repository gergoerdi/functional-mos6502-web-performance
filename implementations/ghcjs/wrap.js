function ghcjs_run(buf){
  var o = {};
  ghcjs_callback_(buf,o);
  return o.ret;
};
