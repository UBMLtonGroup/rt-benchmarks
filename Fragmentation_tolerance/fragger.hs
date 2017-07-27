import Control.Monad.ST
import Data.Array.ST
import Data.Maybe
import Data.Foldable

import           Control.Monad               (replicateM_)
--import           Data.Vector.Unboxed         (freeze)
import qualified Data.Vector.Unboxed.Mutable as V
import           System.Random               (randomRIO)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic         as G
--buildPair n = do arr <- newArray (1,n) (Just 37) :: ST s (STArray s Int (Maybe Int))
--                 a <- readArray arr 1
--                 writeArray arr 1 Nothing
--                 b <- readArray arr 1
--                 return (a,b)



--doit2 kArraySize = do
--        arr <- newArray(1,kArraySize) (Just 37) :: ST s (STArray s Int (Maybe Int))
--        fill arr 1
--        return arr 
--    where
--        fill arr i
--            | i < kArraySize = do
--                            writeArray arr i (Just 2)
--                            fill arr (i+1)
--            | otherwise = return ()

--allocateVector kArraySize = do
--    vector <- V.replicate kArraySize (Just 0 :: Maybe Int)

--    replicateM_ kArraySize $ do
--        i <- randomRIO (0, kArraySize-1)
--        oldCount <- V.read vector i
--        V.write vector i 1

--    ivector <- freeze vector
--    print ivector


doit kArraySize = do
        v <- GM.new kArraySize
        fill v 0
        G.unsafeFreeze v
    where 
        fill v i
            | i < kArraySize = do
                            GM.unsafeWrite v i 1
                            fill v (i+1)
            | otherwise = return ()


main = doit 10
--main = print $ runST (allocateNewArray 10) 
