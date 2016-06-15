module ParseN (parseInput, inputFromFile) where

import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.ReadP


inputFromFile :: Read a => FilePath -> IO (Maybe ([[a]], [[a]]))
inputFromFile fname = do
  str <- readFile fname
  return $ parseInput str


parseInput :: Read a => String -> Maybe ([[a]], [[a]])
parseInput str =
  listToMaybe [ x | (x, "") <- readP_to_S nonogramInput str ]


nonogramInput :: Read a => ReadP ([[a]], [[a]])
nonogramInput = do
  skipMany comment
  string "cols"
  newline
  cols <- num_list `endBy1` newline
  string "rows"
  newline
  rows <- num_list `endBy1` newline
  eof
  return (cols, rows)


comment :: ReadP ()
comment = do
  string "#"
  munch (/= '\n')
  newline
  return ()


-- NOTE: single "0" means empty vector
num_list :: Read a => ReadP [a]
num_list = do
  nums <- number `sepBy1` spaces
  optional spaces
  return $ fmap read $ filter (/= "0") nums


number :: ReadP String
number = do
  digits <- munch1 isDigit
  return digits


spaces :: ReadP ()
spaces = skipMany1 $ (char ' ') <++ (char '\t')


newline :: ReadP ()
newline = skipMany1 $ (string "\n") <++ (string "\r\n")
