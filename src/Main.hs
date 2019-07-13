module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

data Puzzle =
  Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $
     fmap renderPuzzleChar discovered)
    ++ "\nGuessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str undiscovered []
  where undiscovered = map (\x -> Nothing) str

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) c = elem c str

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word
                 filledInSoFar s) c = 
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed 
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c)
            word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
              \ character, pick \
              \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
              \ word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
              \ the word, try again."
      return (fillInCharacter puzzle guess)

incorrectGuesses :: Puzzle -> Int
incorrectGuesses (Puzzle _ discovered guessed) =
  length guessed - (length $ filter isJust discovered)

gameOver :: Puzzle -> IO ()
gameOver pzl@(Puzzle wordToGuess _ guessed) =
  if (incorrectGuesses pzl) > 7 then
    do putStrLn "You lose!"
       putStrLn $
         "The word was: " ++ wordToGuess
       exitSuccess
  else return()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $
    "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   ->
      putStrLn "Your guess must\
              \ be a single character"

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w = 
          let l = length (w :: String)
          in      l >= minWordLength
              &&  l <  maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle =
        freshPuzzle (fmap toLower word)
  runGame puzzle
