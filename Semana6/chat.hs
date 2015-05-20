import Prelude
import Network (listenOn, accept, sClose, Socket, withSocketsDo, PortID(..))
import System.IO
import System.Environment (getArgs)
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forM, filterM, liftM, when)

main :: IO ()
main = withSocketsDo $ do
         port <- (fromIntegral . read . head) `liftM` getArgs 
         socket <- listenOn $ PortNumber port
         putStrLn $ "listening on: " ++ show port
         setup socket `finally` sClose socket

setup :: Socket -> IO ()
setup socket = do
  channel <- atomically newTChan
  forkIO $ acceptClient socket channel
  mainLoop channel []

type Client = (TChan String, Handle)

acceptClient :: Socket -> TChan (TChan String, Handle) -> IO ()
acceptClient socket channel = do
  (cHandle, _host, _port) <- accept socket
  cChan <- atomically newTChan
  cTID  <- forkIO $ worker cHandle cChan
  atomically $ writeTChan channel (cChan, cHandle)
  acceptClient socket channel

worker :: Handle -> TChan String -> IO ()
worker handle channel = listenLoop (hGetLine handle) channel
                           `catch`   (const $ return ())
                           `finally` hClose handle

listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan = sequence_ $ repeat (act >>= atomically . writeTChan chan)

data Event a b = NewClient a | Broadcast b


mainLoop :: TChan (TChan String,Handle) -> [(TChan String,Handle)] -> IO ()
mainLoop channel clients = do
  r <- atomically $ (NewClient `fmap` readTChan channel)
                    `orElse`
                    (Broadcast `fmap` tselect clients)
  case r of
    NewClient (ch,h) -> do putStrLn "new client"
                           mainLoop channel $ (ch,h):clients
    Broadcast line   -> do putStrLn $ "data: " ++ line
                           clients' <- forM clients $
                             \(ch,h) -> do hPutStrLn h line
                                           hFlush h
                                           return [(ch,h)]
                             `catch` const (hClose h >> return [])
                           let dropped = length $ filter null clients'
                           when (dropped > 0) $
                             putStrLn $ "clients lost: " ++ show dropped
                           mainLoop channel $ concat clients'

tselect :: [(TChan a,t)] -> STM a
tselect = foldr orElse retry . map (\(ch, ty) -> readTChan ch)

