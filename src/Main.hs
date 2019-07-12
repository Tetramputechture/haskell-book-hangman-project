module Main where

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

main :: IO ()
main = do
  putStrLn "hello world"
