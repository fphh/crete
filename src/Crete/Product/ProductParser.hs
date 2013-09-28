

module Crete.Product.ProductParser where


import Text.ParserCombinators.Parsec

csvFile :: Char -> Parser [[String]]
csvFile sepChar = endBy (line sepChar) eol

line :: Char -> Parser [String]
line sepChar = sepBy (cell sepChar) (char sepChar)

cell :: Char -> Parser String
cell sepChar = do
  spaces
  quotedCell <|> many (noneOf (sepChar:"\n\r"))

quotedCell :: Parser String
quotedCell = 
    do _ <- char '"'
       content <- many quotedChar
       _ <- char '"' <?> "quote at end of cell"
       return content

quotedChar :: Parser Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
