import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Exception
import Data.Char
import Data.Either


data MyError = BadInput Char
             | BadLuck
             deriving Show

instance Error MyError

main = runReaderT (runErrorT (execStateT loop 0)) 'q' >>=
          putStrLn . (either (show.id) show)
  where
    loop :: StateT Int (ErrorT MyError (ReaderT Char IO)) ()
    loop = do x <- liftIO getChar
              end <- ask
              if isSpace x
                then loop
                else when (x /= end) $ do 
                  when (not (isDigit x)) $ throwError $ BadInput x
                  p <- get
                  let n = fromEnum x - fromEnum '0'
                  let t = p + n
                  when (t == 13) $ throwError BadLuck
                  put $ t
                  loop
