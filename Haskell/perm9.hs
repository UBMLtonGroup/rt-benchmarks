-- Requires the package optparse-applicative

import Data.List

import Data.Time.Clock.POSIX
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Text.Printf
import Numeric

import GHC.Stats

import Options.Applicative
import Data.Monoid ((<>))
import Data.Ratio

import Control.Concurrent

data Arguments = Arguments {
    computeThreads :: Integer,
    computeDepth :: Integer,
    iters :: Integer,
    sleepTime :: Double,
    gcThreads :: Integer,
    permLength:: Int,
    showGCStats :: Bool
}

revLoop :: [a] -> Int -> [a] -> [a]
revLoop _ 0 y = y
revLoop x n y = revLoop (tail x) (n - 1) ((head x) : y)

f :: Int -> ([a], [[a]]) -> ([a], [[a]])
f n (x, perms) = let p = revLoop x n (drop n x) in (p, p : perms)

p :: Int -> ([a], [[a]]) -> ([a], [[a]])
p n t = foldr (.) id (intersperse (f n) $ replicate n (p (n-1))) $ t

sumPerms :: (Num a) => [[a]] -> a
sumPerms perms = sum $ map sum perms

posixTimeToMillis :: POSIXTime -> Integer
posixTimeToMillis =  round . (1000 *)

timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime

timeInMillis :: IO Integer
timeInMillis = (`div` 1000) <$> timeInMicros

timeInSeconds :: IO Integer
timeInSeconds = (`div` 1000) <$> timeInMillis

timeInSeconds' :: IO Double
timeInSeconds' = (/ 1000000) . fromIntegral <$> timeInMicros

formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""

-- Terrible workaround completely dependent on the implementation of show for ThreadId
-- If this breaks just use the ignored parameter to compute and gcFunc
threadIdNum :: ThreadId -> Integer
threadIdNum t = read $ words (show t) !! 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

gcFunc :: (Show a) => Int -> Integer -> (String -> IO ()) -> a -> IO ()
gcFunc len iters printFun threadIdNum = do
    threadDelay . fromIntegral . round $ 30 * 1000000
    let gcLoop i = do
        stats1 <- getGCStats
        threadId <- myThreadId
        tStart <- timeInSeconds'
        --printFun $ "gc:start:" ++ show (threadIdNum threadId) ++  ":" ++ show i ++ ":" ++ show (posixTimeToMillis tStart)
        printFun $ "gc:start:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ formatFloatN tStart 4 ++ ":" ++ show (currentBytesUsed stats1)
        _ <- (evaluate . force) $ let l = [1..len] in sumPerms (snd (p len (l, [l]) ))
        tStop <- timeInSeconds'
        stats2 <- getGCStats
        --printFun $ "gc:stop:" ++ show (threadIdNum threadId) ++  ":" ++ show i ++ ":" ++ show (posixTimeToMillis tStop)
        printFun $ "gc:stop:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ formatFloatN tStop 4 ++ ":" ++ show (currentBytesUsed stats2)


    mapM_ gcLoop [1..iters]

compute :: (Show a) => Integer -> Integer -> Double -> (String -> IO ()) -> a -> IO ()
compute depth iters sleepTime printFun threadIdNum = do
    let compLoop i = do
        stats1 <- getGCStats
        threadId <- myThreadId
        tStart <- timeInSeconds'
        --printFun $ "compute:start:" ++ show (threadIdNum threadId) ++  ":" ++ show i ++ ":" ++ show (posixTimeToMillis tStart)
        printFun $ "compute:start:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ formatFloatN tStart 4 ++ ":" ++ show (currentBytesUsed stats1)
        _ <- (evaluate . force) $ fib depth
        stats2 <- getGCStats
        tStop <- timeInSeconds'
        --printFun $ "compute:stop:" ++ show (threadIdNum threadId) ++  ":" ++ show i ++ ":" ++ show (posixTimeToMillis tStop)
        printFun $ "compute:stop:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ formatFloatN tStop 4 ++ ":" ++ show (currentBytesUsed stats2)
        threadDelay . fromIntegral . round $ sleepTime * 1000000

    mapM_ compLoop [1..iters]

forkThread :: IO () -> IO (MVar ())
forkThread f = do
    isDone <- newEmptyMVar
    _ <- forkFinally f (\_ -> putMVar isDone ())
    return isDone

runBenchmark :: Arguments -> IO ()
runBenchmark (Arguments computeThreads computeDepth iters sleepTime gcThreads permLength showGCStats) = do

    printLock <- newMVar ()
    let concurrentPrint s = withMVar printLock (\_ -> putStrLn s)

    --gcHandles <- mapM (forkThread . gcFunc permLength iters concurrentPrint) [2..(gcThreads + 1)]
    gcHandles <- mapM (forkThread . gcFunc permLength iters concurrentPrint) [1..gcThreads]
    --computeHandles <- mapM (forkThread . compute computeDepth iters sleepTime concurrentPrint) [(gcThreads + 2) .. (gcThreads + computeThreads + 1)]
    computeHandles <- mapM (forkThread . compute computeDepth iters sleepTime concurrentPrint) [1..computeThreads]

    mapM_ takeMVar $ gcHandles ++ computeHandles

    when showGCStats $ do
        stats <- getGCStats
        print stats

benchmarkCLI :: Parser Arguments
benchmarkCLI = Arguments
    -- -t, --compute-threads NUM     1   Compute Threads
    <$> option auto
        ( long "compute-threads"
        <> short 't'
        <> metavar "NUM"
        <> value 1
        <> help "Compute Threads" )

    -- -d, --compute-depth NUM       37  Compute Depth
    <*> option auto
        ( long "compute-depth"
        <> short 'd'
        <> metavar "NUM"
        <> value 37
        <> help "Compute Depth" )

    -- -i, --iterations NUM          10  Compute/GC Iterations
    <*> option auto
        ( long "iterations"
        <> short 'i'
        <> metavar "NUM"
        <> value 10
        <> help "Compute/GC Iterations" )

    -- -s, --compute-sleep NUM       1   Compute Sleep
    <*> option auto
        ( long "compute-sleep"
        <> short 's'
        <> metavar "NUM"
        <> value 1.0
        <> help "Compute Sleep" )

    -- -g, --gc-threads NUM          1   GC Threads
    <*> option auto
        ( long "gc-threads"
        <> short 'g'
        <> metavar "NUM"
        <> value 1
        <> help "GC Threads" )

    -- -e, --tree-depth NUM          10  Maximum tree depth to allocate
    <*> option auto
        ( long "perm-depth"
        <> short 'e'
        <> metavar "NUM"
        <> value 9
        <> help "Size of list to generate permutations of" )

    -- -S, --gc-stats                    Print GC stats
    <*> switch
        ( long "gc-stats"
        <> short 'S'
        <> help "Print GC stats. Requires the flags +RTS -T -RTS" )

main :: IO ()
main = execParser opts >>= runBenchmark
    where
     opts = info (helper <*> benchmarkCLI)
       ( fullDesc
      <> progDesc "Multithreaded Haskell Perm9"
       )
