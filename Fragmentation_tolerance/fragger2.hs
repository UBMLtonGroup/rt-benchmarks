{-# LANGUAGE BangPatterns #-}
import Data.Array
import Control.Monad
import Control.DeepSeq
import Data.IORef
import GHC.Stats
--allocateArray :: Int -> Array Int Int
allocateArray num = do
    array (1, 1) [(1, num)]

increment :: (Integral a) => a -> a
increment i = i + 1


main = do
    let arr = []
    lst <- newIORef arr
    forM_ [1..10] (\a -> do 
        --let arr = seq q ( q : arr) where
        --        q = (allocateArray a)
        modifyIORef' lst ((allocateArray a) :)
        
        b <-  readIORef lst
        stats <- getGCStats
        print (currentBytesUsed stats)
        --print (length b)
        print a)
    return ()
   -- print myArray
    --print $ myArray ! 1
    --print $ bounds myArray
    --print $ indices myArray
    --print $ elems myArray
    --print $ assocs myArray
