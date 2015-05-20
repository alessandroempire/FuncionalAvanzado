{-# LANGUAGE ScopedTypeVariables #-}
import System.IO
import Control.Exception
import Control.Concurrent
import Network
import System.Posix

main = do
  installHandler sigPIPE Ignore Nothing
  s <- listenOn (PortNumber 9900)
  acceptConnections s

acceptConnections sock = do
  conn@(h,host,port) <- accept sock
  forkIO $ catch (talk conn `finally` hClose h) (\(e :: SomeException) -> print e)
  acceptConnections sock

talk conn@(h,_,_) = do
  hGetLine h >>= hPutStrLn h
  hFlush h
  talk conn
