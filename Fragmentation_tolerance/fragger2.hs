{-# LANGUAGE BangPatterns #-}

-- ghc fragger2.hs -rtsopts -XFlexibleContexts
-- ./fragger2 478510 +RTS -T -M39231489 -s -RTS

import Data.Array as AR
import Data.Array.IO as MA
import Control.Monad
import Control.DeepSeq
import Data.IORef
import GHC.Stats
import Data.Sequence
import Data.Maybe
import System.Mem
import System.Environment
import Control.Concurrent
import System.IO
import Data.Time.Clock.POSIX


--allocateArray :: Int -> Array Int Int
allocateArray num = do
    Just (AR.array (1, 1) [(1, num)])

increment :: (Integral a) => a -> a
increment i = i + 1

fillHeap :: Int -> IO (IOArray Int (Maybe (Array Integer Int)))
fillHeap n = do
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

fragmentHeap :: IOArray Int (Maybe (Array Integer Int)) -> Int -> Int -> IO (IOArray Int (Maybe (Array Integer Int)))
fragmentHeap lref n size
    | n <= size = do 
                          writeArray lref n Nothing
                          putProgress n
                          fragmentHeap lref (n+2) size
    | otherwise = return lref

putProgress :: Int -> IO ()
putProgress s = hPutStr stdout $ "\r\ESC[K" ++ show s

main = do
    args <- getArgs
    let size = read $ args!!0 :: Int -- 478510 --407262
    --lstref <- fillHeap size
    putStrLn "fillHeap"
    arr <- fillHeap size
    putStrLn "fragmentHeap"
    --lst <- readIORef lstref
    --print (Prelude.length lst)
    --stats <- getGCStats
    --print $ "Bytes allocated " ++ show(maxBytesUsed stats)
    --let seq = fromList lst
    --seqref <- newIORef seq
    --performGC
    --threadDelay 100000
    arr <- fragmentHeap arr 2 size
    putStrLn "\nmake small array"
    --b <- readArray arr 2
    --print b
    --readIORef lstref >>= print
    tStart <- getPOSIXTime
    arr2 <- fillHeap (20000)
    tStop <- getPOSIXTime
    traverseArray arr2 1 20000
    putStrLn $ "took " ++ show (tStop - tStart)
    --let arr2 = AR.array (1, 650000) [(i,i) | i <- [1..650000]]
    -- arr2 <-  fillHeap(200)
    --treadArray arr2 1 650000
    stats <- getGCStats
    putStrLn $ "Bytes allocated " ++ show(maxBytesUsed stats)
    putStrLn ("done")

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
