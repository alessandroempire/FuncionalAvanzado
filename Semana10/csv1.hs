import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf "\",\n\r")

quotedCell = do
  char '"'
  content <- many quotedChar
  char '"' <?> "quote at end of cell"
  return content

quotedChar = 
  noneOf "\"" <|> try (string "\"\"" >> return '"') 

eol = try (string "\n\r") 
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"

parseCSV = parse csvFile

main = do input <- getContents
          case parseCSV "(stdin)" input of
            Left c  -> do putStrLn "Error: "
                          print c
            Right r -> mapM_ print r

goodcsv = "Campo 1,Campo 2,Campo 3\nfoo,bar,baz\nfee,fai,foe\ncurly,larry,moe\n"
badcsv = "Campo 1,Campo 2,Campo 3\nfoo,bar\nfee,fai,foe\ncurly,larry,moe\n"
