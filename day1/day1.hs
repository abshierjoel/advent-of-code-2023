import Control.Monad
import Data.Char
import System.IO

fileName :: String
fileName = "input.txt"

main = do
  contents <- readFile fileName
  print . sum . map (combineDigits . getChars) . words $ contents

combineDigits :: (Char, Char) -> Int
combineDigits (ones, tens) = read [ones, tens]

getChars :: String -> (Char, Char)
getChars str = do
  let firstChar = first str
  let lastChar = first (reverse str)

  (firstChar, lastChar)

first :: String -> Char
first s =
  if isDigit (head s) then head s else first (tail s)
