import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq

deep :: NFData a => a -> a
deep a = deepseq a a

parMapper :: (a -> b) -> [a] -> Eval [b]
parMapper f []     = return []
parMapper f (a:as) = do
  b  <- rpar (f a)
  bs <- parMapper f as
  return (b:bs)

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    evaluate $ deep $ runEval $ parMapper solve grids
    return ()

