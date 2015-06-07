module Main where

import Prelude hiding (catch)

import Control.Exception ( SomeException(..),
                           AsyncException(..)
                         , catch, handle, throw)
import Control.Monad (forever)
import System.IO
import System.Posix.Signals

repl :: IO ()
repl = forever $ do
    putStr ">>> " >> hFlush stdout
    out <- getLine
    if null out
       then return ()
       else putStrLn out

reportSignal :: IO ()
reportSignal = do putStrLn "\nkeyboardSignal"
                  

main = do
    _ <- installHandler keyboardSignal (Catch reportSignal) Nothing
    handle onAbort repl
    putStrLn "Exiting..."

onAbort e = do
    let x = show (e :: SomeException)
    putStrLn $ "\nAborted: " ++ x
