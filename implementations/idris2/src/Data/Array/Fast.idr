module Data.Array.Fast

import JS.Array

-- reads a value at the given index from a byte array
-- I reimplemented this here, because the version from
-- the dom library returns a Maybe (something we don't need
-- here), and has a call to `<$>`, which will conjure a
-- Monad IO record (something I should fix).
%foreign "javascript:lambda:(_a, arr,n,w) => arr[n]"
prim__readArr : Array a -> Bits32 -> PrimIO a

-- writes a value to a mutable array
%foreign "javascript:lambda:(_a, arr,n,v,w) => { arr[n] = v }"
prim__writeArr : Array a -> Bits32 -> a -> PrimIO ()

public export
readArray : Array a -> Bits32 -> IO a
readArray arr idx = fromPrim $ prim__readArr arr idx

public export
writeArray : Array a -> Bits32 -> a -> IO ()
writeArray arr idx x = fromPrim $ prim__writeArr arr idx x
