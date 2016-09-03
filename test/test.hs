import Control.Monad
import Online.Quantiles
import qualified Control.Foldl as L
import qualified Data.Vector as V
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWCD

-- * testing
tU :: Int -> IO Quantiles
tU x' = do
  rv <- rvU 1000
  return $ L.fold (quantiles 5) (take x' rv)

testU1000 :: IO Bool
testU1000 = do
  res <- tU 1000 
  return $ res == Quantiles 5 (V.fromList [2.8392930116749593e-4,0.24096695011021912,0.48938466145946324,0.7206907810213142,0.9964338292892708]) (V.fromList [1,250,500,750,1000])

rvInts :: (Int,Int) -> Int -> IO [Int]
rvInts range n = do
    mwc <- MWC.initialize $ V.fromList [42]
    replicateM n (MWC.uniformR range mwc)

rvN :: Int -> IO [Double]
rvN n = do
    mwc <- MWC.initialize $ V.fromList [42]
    replicateM n (MWCD.standard mwc)

rvU :: Int -> IO [Double]
rvU n = do
    mwc <- MWC.initialize $ V.fromList [42]
    replicateM n (MWC.uniform mwc)

main :: IO ()
main = print =<< tU 1000