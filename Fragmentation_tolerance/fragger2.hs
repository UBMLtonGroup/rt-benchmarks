{-# LANGUAGE BangPatterns #-}
import Data.Array
import Control.Monad
import Control.DeepSeq
import Data.IORef
import GHC.Stats
--allocateArray :: Int -> Array Int Int
allocateArray2 num = do 
    stats <- getGCStats
    let a = array (1, 1) [(1, (bytesAllocated stats))]
    return a

allocateArray num = do
    array (1, 1) [(1, num)]

increment :: (Integral a) => a -> a
increment i = i + 1

-- case xs of [] -> foo; _:_ -> foo.

main = do
    let arr = []
    lst <- newIORef arr
    forM_ [1..1000] (\a -> do 
        --let arr = seq q ( q : arr) where
        --        q = (allocateArray a)
        modifyIORef' lst ((allocateArray a) :)
        
        b <-  readIORef lst
        stats <- getGCStats
        print $ "heap alloc " ++ show (bytesAllocated stats) ++ " live " ++ show (maxBytesUsed stats)
        --print $ "len " ++ show (length b)
        print a
        )
    return ()
   -- print myArray
    --print $ myArray ! 1
    --print $ bounds myArray
    --print $ indices myArray
    --print $ elems myArray
    --print $ assocs myArray
