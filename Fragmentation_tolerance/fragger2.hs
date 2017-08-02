{-# LANGUAGE BangPatterns #-}
import Data.Array as AR
import Data.Array.IO as MA
import Control.Monad
import Control.DeepSeq
import Data.IORef
import GHC.Stats
import Data.Sequence
import Data.Maybe
import System.Mem
import Control.Concurrent
--allocateArray :: Int -> Array Int Int
allocateArray num = do
    Just (AR.array (1, 1) [(1, num)])

increment :: (Integral a) => a -> a
increment i = i + 1


fillHeap n= do
    --let ls = empty
    --lsref <- newIORef ls
    arr <- MA.newArray (1,n) Nothing :: IO (IOArray Int (Maybe(Array i e )))
    traverseArray arr 1 n
    --forM_ [1..n] (\a -> do
            ----modifyIORef' lsref $! (((<|) (allocateArray a)) ) 
            --MA.writeArray arr a $!(allocateArray a) )
    --return lsref
    return arr
 
{-fragmentHeap lref n = do 
    forM_ [1..n] (\a -> do
                if (even a) 
                    then do
                        --print a
                        --modifyIORef' lref $! ((update a Nothing) ) 
                        writeArray lref a Nothing
                    else return () )
-}

traverseArray arr n size
    | n <= size = do
                    MA.writeArray arr n $!(allocateArray n) 
                    traverseArray arr (n+1) size
    | otherwise = return arr

treadArray arr n size
    | n<=size = do
            print $ arr ! n
            treadArray arr (n+1) size
    | otherwise = return ()

fragmentHeap lref n size
    | n <= size = do 
                          writeArray lref n Nothing
                          print n
                          fragmentHeap lref (n+2) size
    | otherwise = return lref

main = do
    let size = 478510--407262
    --lstref <- fillHeap size
    print "ok0"
    arr <- fillHeap size
    print "ok1"
    --lst <- readIORef lstref
    --print (Prelude.length lst)
    --stats <- getGCStats
    --print $ "Bytes allocated " ++ show(maxBytesUsed stats)
    --let seq = fromList lst
    --seqref <- newIORef seq
    --performGC
    --threadDelay 100000
    arr <- fragmentHeap arr 2 size
    print "ok2"
    --b <- readArray arr 2
    --print b
    --readIORef lstref >>= print
    --arr2 <- fillHeap (200)
    --let arr2 = AR.array (1, 650000) [(i,i) | i <- [1..650000]]
    -- arr2 <-  fillHeap(200)
    --treadArray arr2 1 650000
    stats <- getGCStats
    print $ "Bytes allocated " ++ show(maxBytesUsed stats)
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
