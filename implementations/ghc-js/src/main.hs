{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Driver

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import qualified Data.ByteString as BS
import Control.Monad (replicateM_)

foreign import javascript "((p,o,n) => loadFile(h$decodeUtf8(p,n,o)))" loadFile_ :: Ptr CChar -> Int -> IO (Ptr CChar)
foreign import javascript "((p) => p.len)" bufferLength_ :: Ptr CChar -> IO Int
foreign import javascript "logStart" logStart :: IO ()
foreign import javascript "logEnd" logEnd :: Int -> IO ()
foreign import javascript "numRuns" numRuns :: IO Int

loadFile :: String -> IO CStringLen
loadFile fileName = withCStringLen fileName \(ptr, len) -> do
    ptr' <- loadFile_ ptr len
    len' <- bufferLength_ ptr'
    return (ptr', len')

timed :: Bool -> IO Int -> IO Int
timed False act = act
timed True act = do
    logStart
    res <- act
    logEnd res
    return res

-- foreign export javascript "run" run' :: Ptr CChar -> Int -> IO Int
run' :: Bool -> Ptr CChar -> Int -> IO Int
run' measure ptr len = do
    bs <- BS.packCStringLen (ptr, len)
    timed measure $ run bs

main :: IO ()
main = do
    cnt <- numRuns
    putStrLn "Warmup"
    replicateM_ cnt $ uncurry (run' False) =<< loadFile "data/program.dat"
    putStrLn "Measure"
    replicateM_ cnt $ uncurry (run' True) =<< loadFile "data/program.dat"
