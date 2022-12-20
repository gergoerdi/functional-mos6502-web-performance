{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Driver

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import qualified Data.ByteString as BS

foreign import javascript "((p,o,n,p2) => loadFile(h$decodeUtf8(p,n,o),p2))" loadFile_ :: Ptr CChar -> Int -> Ptr CChar -> IO ()
foreign import javascript "((p) => p.len)" bufferLength_ :: Ptr CChar -> IO Int

loadFile :: String -> IO BS.ByteString
loadFile fileName = withCStringLen fileName \(ptr, len) -> do
    allocaBytes 100000 \ptr' -> do
        loadFile_ ptr len ptr'
        len' <- bufferLength_ ptr'
        BS.packCStringLen (ptr', len')

main :: IO ()
main = do
    bs <- loadFile "data/program.dat"
    print =<< run bs
