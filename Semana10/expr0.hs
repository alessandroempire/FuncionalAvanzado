module Parser where
  
import Text.ParserCombinators.Parsec
  
expr :: Parser Integer
expr = do t <- term
          do try (char '+') >> expr >>= return . (t+)
             <|> (try (char '-') >> expr >>= return . (t-))
             <|> return t
    <?> "expr"

term = do p <- power
          do try (char '*') >> term >>= return . (p*)
             <|> (try (char '/') >> term >>=  return . (div p))
             <|> return p
    <?> "term"

power = do b <- factor 
           do try (char '^') >> power >>= return . (b^)
              <|> return b
     <?> "power"

factor = do char '('
            e <- expr
            char ')'
            return e
      <|> number
      <?> "factor"
  
number = do ds <- many1 digit
            return (read ds)
      <?> "number"

