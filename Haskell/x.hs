import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

main =
  do start <- getTime Monotonic
     evaluate (sum [1 .. 1000000])
     end <- getTime Monotonic
     fprint (timeSpecs % "\n") start end
