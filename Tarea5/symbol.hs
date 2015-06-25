{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec
import Control.Applicative

data Product = Mouse | Keyboard | Monitor | Speakers deriving Show

productParser :: Parser Product
productParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)

main :: IO ()
main = do
  print $ parseOnly symbolParser "true"
  print $ parseOnly productParser "mouze"
  print $ parseOnly productParser "monitor"
  print $ parseOnly productParser "keyboard"


data Symbol = SymTrue | SymFalse | SymAnd | SymOr | SymXor 
    deriving (Show, Eq)

symbolParser :: Parser Symbol
symbolParser = (string "true"  >> return SymTrue)
           <|> (string "false" >> return SymFalse)
           <|> (string "and"   >> return SymAnd)
           <|> (string "or"    >> return SymOr)
           <|> (string "xor"   >> return SymXor)
