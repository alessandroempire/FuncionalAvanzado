import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq

deep :: NFData a => a -> a
deep a = deepseq a a

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    evaluate $ deep ( map solve grids `using` parList rseq)
    return ()

