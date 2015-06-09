{-# LANGUAGE BangPatterns #-}
import Data.Array.Repa
import System.Environment

type Weight  = Int
type Graph r = Array r DIM2 Weight

shortestPaths :: Graph U -> Graph U
shortestPaths g0 = go g0 0
  where
   Z :. _ :. n = extent g0
   go !g !k | k == n    = g
            | otherwise = let g' = computeS $ fromFunction (Z:.n:.n) sp
                          in go g' (k+1)
                          where
                            sp (Z:.i:.j) = min (g ! (Z:.i:.j))
                                               (g ! (Z:.i:.k) +
                                                g ! (Z:.k:.j))

main = do
   [n] <- fmap (fmap read) getArgs
   let g = fromListUnboxed (Z:.n:.n) [0..n^(2::Int)-1] :: Graph U
   print (sumAllS (shortestPaths g))
