{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Driver

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import qualified Data.ByteString as BS

foreign import javascript "((p,o,n) => loadFile(h$decodeUtf8(p,n,o)))" loadFile_ :: Ptr CChar -> Int -> IO (Ptr CChar)
foreign import javascript "((p) => p.len)" bufferLength_ :: Ptr CChar -> IO Int

loadFile :: String -> IO CStringLen
loadFile fileName = withCStringLen fileName \(ptr, len) -> do
    ptr' <- loadFile_ ptr len
    len' <- bufferLength_ ptr'
    return (ptr', len')

main :: IO ()
main = do
    bs <- BS.packCStringLen =<< loadFile "data/program.dat"
    print =<< run bs
