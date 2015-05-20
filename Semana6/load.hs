import Control.Concurrent
import Control.Monad

worker ch = forever $ do
  v <- readFile "/proc/loadavg"
  writeChan ch v
  threadDelay (10^6)

main = do
  ch <- newChan
  forkIO $ worker ch
  xs <- getChanContents ch
  mapM_ putStr xs
