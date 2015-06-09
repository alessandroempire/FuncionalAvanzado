import Text.ParserCombinators.Parsec

csv :: GenParser Char st [[String]]
csv = do r <- many line
         eof
         return r

line :: GenParser Char st [String]
line = do r <- cells
          eol
          return r

cells :: GenParser Char st [String]
cells = do f <- content
           n <- moreCells
           return (f:n)

moreCells :: GenParser Char st [String]
moreCells = (char ',' >> cells) <|> (return [])

content :: GenParser Char st String
content = many $ noneOf ",\n"

eol :: GenParser Char st Char
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csv "csv"

goodcsv = "Campo 1,Campo 2,Campo 3\nfoo,bar,baz\nfee,fai,foe\ncurly,larry,moe\n"
badcsv = "Campo 1,Campo 2,Campo 3\nfoo,bar\nfee,fai,foe\ncurly,larry,moe\n"
