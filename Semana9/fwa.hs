{-# LANGUAGE BangPatterns #-}
import Control.Monad.Identity
import Data.Array.Accelerate                as A
import Data.Array.Accelerate.CUDA
import System.Environment

type Weight = Int32
type Graph  = Array DIM2 Weight

step :: Acc (Scalar Int) -> Acc Graph -> Acc Graph
step k g = generate (shape g) sp
  where k' = the k
        sp :: Exp DIM2 -> Exp Weight
        sp ix = let (Z:.i:.j) = unlift ix
                in min (g ! (index2 i j))
                        (g ! (index2 i k') +
                         g ! (index2 k' j))

shortestPathsAcc :: Int -> Acc Graph -> Acc Graph
shortestPathsAcc n g0 = foldl1 (>->) steps $ g0
  where
    steps :: [ Acc Graph -> Acc Graph ]
    steps = [ step (unit (constant k)) | k <- [0..n-1] ]

shortestPaths :: Graph -> Graph
shortestPaths g0 = run (shortestPathsAcc n (use g0))
  where
    Z :. _ :. n = arrayShape g0

main :: IO ()
main = do
   (n:_) <- fmap (fmap read) getArgs
   print (run (let g :: Acc Graph
                   g    = generate (constant (Z:.n:.n) :: Exp DIM2) f

                   f :: Exp DIM2 -> Exp Weight
                   f ix = let i,j :: Exp Int
                              Z:.i:.j = unlift ix
                          in
                          A.fromIntegral j +
                           A.fromIntegral i * constant (Prelude.fromIntegral n)
               in
               A.foldAll (+) (constant 0) (shortestPathsAcc n g)))
