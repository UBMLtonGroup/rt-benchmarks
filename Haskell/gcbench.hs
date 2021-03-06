-- Requires the package optparse-applicative

-- http://chrisdone.com/posts/measuring-duration-in-haskell

import Data.Time.Clock.POSIX
import Control.DeepSeq
import Control.Monad
import Control.Exception (evaluate)
import Text.Printf
import Numeric

import GHC.Stats
import System.Clock as Clock

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
    gcDelay:: Integer,
    treeDepth:: Integer,
    showGCStats :: Bool
}

-- ! indicates that the type should be strictly evaluated. Without it tree will not be assembled unless traversed
data Node = Empty | Node {leftNode :: !Node, rightNode :: !Node}

-- Allows us to force the evaluation of Node result types
-- Use of force (and by extension implementation of NFData) may be unnecessary
instance NFData Node where
     rnf a = a `seq` ()

emptyNode :: () -> Node
emptyNode () = Node {leftNode = Empty, rightNode = Empty}

nonEmptyNode :: Node -> Node -> Node
nonEmptyNode left right = Node {leftNode = left, rightNode = right}

makeTree :: Integer -> Node
makeTree iDepth =
    if iDepth <= 0 then emptyNode()
        else nonEmptyNode (makeTree $ iDepth-1) (makeTree $ iDepth-1)

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
-- threadIdNum :: ThreadId -> Integer
-- threadIdNum t = read $ words (show t) !! 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

simpleloop :: Integer -> Integer
simpleloop 0 = 12344321
simpleloop n =
  do
    simpleloop (n-1)


gcFunc :: (Show a) => Integer -> Integer -> (String -> IO ()) -> Integer -> a -> IO ()
gcFunc depth iters printFun gcDelay threadIdNum = do
    threadDelay . fromIntegral $ gcDelay * 1000000
    longLivedArray <- evaluate . force $ ([1..1000] :: [Integer])
    longLivedTree <- evaluate . force $ makeTree depth

    let gcLoop i = do
        stats1 <- getGCStats
        threadId <- myThreadId
        tStart <- getPOSIXTime --timeInMicros
        printFun $ "gc:start:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ show tStart ++ ":" ++ show (currentBytesUsed stats1)
        _ <- (evaluate . force) $ makeTree depth
        tStop <- getPOSIXTime --timeInMicros
        stats2 <- getGCStats
        printFun $ "gc:stop:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ show tStop ++ ":" ++ show (currentBytesUsed stats2)

    mapM_ gcLoop [1..iters]

    _ <- evaluate . force $ longLivedArray
    _ <- evaluate . force $ longLivedTree
    return ()

compute :: (Show a) => Integer -> Integer -> Double -> (String -> IO ()) -> a -> IO ()
compute depth iters sleepTime printFun threadIdNum = do
    let compLoop i = do
        stats1 <- getGCStats
        threadId <- myThreadId
        tStart <-  getPOSIXTime -- timeInMicros

        --printf "%d\n" tStart
        printFun $ "compute:start:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ show tStart ++ ":" ++ show (currentBytesUsed stats1)
        -- _ <- (evaluate . force) $ simpleloop depth
        let r = simpleloop depth
        tStop <- r `deepseq` getPOSIXTime --timeInMicros
        stats2 <- getGCStats

        printFun $ "compute:stop:" ++ show (threadIdNum) ++  ":" ++ show i ++ ":" ++ show tStop ++ ":" ++ show (currentBytesUsed stats2)
        -- threadDelay . fromIntegral . round $ sleepTime * 1000000

    mapM_ compLoop [1..iters]

forkThread :: IO () -> IO (MVar ())
forkThread f = do
    isDone <- newEmptyMVar
    _ <- forkFinally f (\_ -> putMVar isDone ())
    return isDone

runBenchmark :: Arguments -> IO ()
runBenchmark (Arguments computeThreads computeDepth iters sleepTime gcThreads gcDelay treeDepth showGCStats) = do

    printLock <- newMVar ()
    let concurrentPrint s = withMVar printLock (\_ -> putStrLn s)

    gcHandles <- mapM (forkThread . gcFunc treeDepth iters concurrentPrint gcDelay) [1..gcThreads]
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

    -- -J, --gc-delay NUM          60   GC startup delay (secs)
    <*> option auto
        ( long "gc-delay"
        <> short 'J'
        <> metavar "NUM"
        <> value 60
        <> help "GC Startup delay" )

    -- -e, --tree-depth NUM          10  Maximum tree depth to allocate
    <*> option auto
        ( long "tree-depth"
        <> short 'e'
        <> metavar "NUM"
        <> value 10
        <> help "Maximum tree depth to allocate" )

    -- -S, --gc-stats                    Print GC stats
    <*> switch
        ( long "gc-stats"
        <> short 'S'
        <> help "Print GC stats" )

main :: IO ()
main = execParser opts >>= runBenchmark
    where
     opts = info (helper <*> benchmarkCLI)
       ( fullDesc
      <> progDesc "Multithreaded Haskell GCBench"
       )
