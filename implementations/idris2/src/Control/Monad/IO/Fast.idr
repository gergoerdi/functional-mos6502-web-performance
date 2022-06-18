module Control.Monad.IO.Fast

%default total

-- Unfortunately, lots of Functor/Applicative/Monad stuff in
-- the Prelude is not specialized for IO, with the exception of
-- `(>>=)`. This is definitely something we should address in
-- the Prelude itself, but for the time being, we hide the
-- problematic Prelude stuff and reimplement it here specialized
-- for `IO`.
%hide Prelude.map
%hide Prelude.(<$>)
%hide Prelude.(>>)
%hide Prelude.ignore
%hide Prelude.(=<<)

export %inline
(<$>) : (a -> b) -> IO a -> IO b
f <$> io = io >>= \va => pure (f va)

export %inline
(=<<) : (a -> IO b) -> IO a -> IO b
f =<< io = io >>= f

-- Thus must *not* be inlined, otherwise the first
-- IO action may be ignored (and not run) altogether.
export
(>>) : IO () -> Lazy (IO b) -> IO b
ia >> ib = ia >>= \_ => ib

-- Thus must *not* be inlined, otherwise the first IO
-- action may be ignored (and not run) altogether.
export
ignore : IO a -> IO ()
ignore io = io >>= \_ => pure ()
