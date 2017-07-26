{-# LANGUAGE BangPatterns #-}
import Data.Array
import Control.Monad
import Control.DeepSeq
import Data.IORef
import GHC.Stats
import Data.Sequence
import Data.Maybe
import System.Mem
--allocateArray :: Int -> Array Int Int
allocateArray num = do
    Just (array (1, 1) [(1, num)])

increment :: (Integral a) => a -> a
increment i = i + 1


fillHeap n= do
    let ls = empty
    lsref <- newIORef ls
    forM_ [0..n] (\a -> do
            --print a
            modifyIORef' lsref $! (((<|) (allocateArray a)) ) )
    return lsref
 
fragmentHeap lref n = do 
    forM_ [0..n] (\a -> do
                if (even a) 
                    then do
                        modifyIORef' lref $! ((update a Nothing) ) 
                    else return () )
               
                

main = do
    let size =200000 -- 407262
    lstref <- fillHeap size
    lst <- readIORef lstref
    print (Prelude.length lst)
   -- stats <- getGCStats
    --let seq = fromList lst
    --seqref <- newIORef seq
    fragmentHeap lstref size
    --readIORef lstref >>= print
    --print $ "Bytes allocated " ++ show(maxBytesUsed stats)
    print ("done")

{- main = do
    let arr = []
    lst <- newIORef arr
    forM_ [1.10] (\a -> do 
        --let arr = seq q ( q : arr) where
        --        q = (allocateArray a)
        modifyIORef' lst $!((allocateArray a) : )
        
        b <-  readIORef lst
        stats <- getGCStats
        print $ "Bytes allocated " ++ show(maxBytesUsed stats)
        print (head b ! 1)
        print a)
    return ()
   -- print myArray
    --print $ myArray ! 1
    --print $ bounds myArray
    --print $ indices myArray
    --print $ elems myArray
    --print $ assocs myArray-}
