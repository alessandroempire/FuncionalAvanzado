import Control.Concurrent
import Control.Monad
import Data.Char

main = do
  ch <- newChan
  cs <- getChanContents ch
  forkIO $ producer ch
  consumer cs

producer c = forever $ do
  key <- getChar
  writeChan c key

consumer = mapM_ putChar . map shift
  where shift c | isAlpha c = chr (ord c + 1)
                | otherwise = c
