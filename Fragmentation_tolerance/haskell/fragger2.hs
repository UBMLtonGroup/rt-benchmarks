{-# LANGUAGE BangPatterns #-}

-- ghc fragger2.hs -rtsopts -XFlexibleContexts -fforce-recomp
-- ./fragger2 233570 +RTS -T -M32477184 -RTS
import Data.Array as AR
import Data.Array.IO as MA
import Control.Monad
import Control.DeepSeq
import Data.IORef
import GHC.Stats as GC
import Data.Sequence
import Data.Maybe
import System.Mem
import System.Environment
import Control.Concurrent
import System.IO
--import Criterion.Measurement as CM
import Data.Time.Clock.POSIX
import System.CPUTime

data SmallObj = SmallArray (IOArray Int Int) | Void
--allocateArray :: Int -> IO (IOArray Int Int)  
allocateArray num = do
    smallarr <- MA.newArray (1,2) num
    return smallarr

increment :: (Integral a) => a -> a
increment i = i + 1

--fillHeap :: Int -> IO (IOArray Int (Maybe (IO(IOArray Int Int))))
fillHeap n = do
    temparr <- MA.newArray(1,2) 10
    let tempsarr = SmallArray (temparr)
    arr <- MA.newArray (1,n) tempsarr :: IO (IOArray Int SmallObj)
    traverseArray arr 1 n
    return arr
 

traverseArray arr n size
    | n <= size = do
                    smallarr <- allocateArray n
                    let ssmallarr = SmallArray (smallarr)
                    MA.writeArray arr n ssmallarr
                    traverseArray arr (n+1) size
    | otherwise = return arr

treadArray arr n size
    | n<=size = do
            --putProgress $ arr ! n
            --b <- readArray arr n
            writeArray arr n 11
            --putProgress b
            treadArray arr (n+1) size
    | otherwise = return ()

goThroughArray arr n size
    | n<=size = do
            b <- readArray arr n
            goThroughArray arr (n+1) size
    | otherwise = return ()

--fragmentHeap :: IOArray Int (Maybe (IOArray Int Int)) -> Int -> Int -> IO (IOArray Int (Maybe (IOArray Int Int)))
fragmentHeap lref n size
    | n <= size = do 
                          writeArray lref n Void
                          --putProgress n
                          fragmentHeap lref (n+2) size
    | otherwise = return lref

putProgress :: Int -> IO ()
putProgress s = hPutStr stdout $ "\r\ESC[K" ++ show s

main = do
    args <- getArgs
    let size = 500000 --read $ args!!0 :: Int -- 233570
    --lstref <- fillHeap size
    --putStrLn "fillHeap"
    arr <- fillHeap size
    --putStrLn "fragmentHeap"
    --stats <- getGCStats
    --print $ "Bytes allocated " ++ show(maxBytesUsed stats)
    --performGC
    --threadDelay 100000
    stats <- GC.getGCStats
    putStrLn $ show(currentBytesUsed stats)
    
    tStart <- getPOSIXTime
    arr <- fragmentHeap arr 2 size
    --putStrLn "\nmake small array"
    arr2 <- MA.newArray(1,3832957) 10 :: IO (IOUArray Int Int)
    
    treadArray arr2 1 3832957
    tStop <- getPOSIXTime
    putStrLn $ show (tStop - tStart)

    goThroughArray arr 1 size    
    --stats <- GC.getGCStats
    --putStrLn $ "Bytes allocated " ++ show(currentBytesUsed stats)
    --putStrLn ("done")

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
