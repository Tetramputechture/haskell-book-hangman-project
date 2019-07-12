module Main where

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w = 
          let l = length (w :: String)
          in      l >= minWordLength
              &&  l <  maxWordLength

main :: IO ()
main = do
  putStrLn "hello world"
