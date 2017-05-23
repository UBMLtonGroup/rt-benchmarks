--import System.Clock
import qualified System.Clock as Clock


main = do
   x <- Clock.getTime Clock.Monotonic
   putStrLn $ ""  ++ show (Clock.toNanoSecs x)


