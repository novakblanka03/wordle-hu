{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Random (randomRIO)
import Control.Monad (unless)
import System.IO (hSetEncoding, utf8, stdout, stdin)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Feedback = Igen | Majdnem | Nem deriving (Show, Eq)
data GameState = GameState { secretWord :: String, guesses :: [(String, [(Char, Feedback)])], maxAttempts :: Int }

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  difficulty <- selectDifficulty
  animals <- loadWords (difficultyFile difficulty)
  gameState <- initGame animals
  gameLoop gameState

data Difficulty = Kezdo | Kozepes | Halado | Profi | Nehez deriving (Show, Eq)

selectDifficulty :: IO Difficulty
selectDifficulty = do
  putStrLn "Válassza ki a nehézségi szintet:"
  putStrLn "1. Kezdő (3 betűs szavak)"
  putStrLn "2. Közepes (4 betűs szavak)"
  putStrLn "3. Haladó (5 betűs szavak)"
  putStrLn "4. Profi (6 betűs szavak)"
  putStrLn "5. Nehéz (7 betűs szavak)"
  choice <- getLine
  case choice of
    "1" -> return Kezdo
    "2" -> return Kozepes
    "3" -> return Halado
    "4" -> return Profi
    "5" -> return Nehez
    _   -> do
      putStrLn "Érvénytelen választás, kérjük próbálja újra."
      selectDifficulty

difficultyFile :: Difficulty -> FilePath
difficultyFile Kezdo  = "./src/kezdo.txt"
difficultyFile Kozepes = "./src/kozepes.txt"
difficultyFile Halado = "./src/halado.txt"
difficultyFile Profi  = "./src/profi.txt"
difficultyFile Nehez  = "./src/nehez.txt"

loadWords :: FilePath -> IO [String]
loadWords path = do
  content <- readFile path
  return (lines content)

initGame :: [String] -> IO GameState
initGame animals = do
  index <- randomRIO (0, length animals - 1)
  let secret = animals !! index
  putStrLn secret
  return $ GameState secret [] 6

getUserInput :: IO String
getUserInput = do
  putStrLn "Írja be a tippjét: "
  getLine

checkGuess :: String -> String -> [(Char, Feedback)]
checkGuess secret guess = map check (T.zip (toText secret) (toText guess))
  where
    check (s, g)
      | s == g    = (g, Igen)
      | g `elem` secret = (g, Majdnem)
      | otherwise = (g, Nem)

myZip :: T.Text -> T.Text -> [(Char, Char)]
myZip t1 t2 = zip (T.unpack t1) (T.unpack t2)

updateGameState :: GameState -> String -> [(Char, Feedback)] -> GameState
updateGameState gameState guess feedback =
  gameState { guesses = (guess, feedback) : guesses gameState }

displayState :: GameState -> IO ()
displayState gameState = do
  putStrLn "Eddigi tippek:"
  mapM_ printGuess (guesses gameState)
  where
    printGuess (guess, feedback) = putStrLn (guess ++ " " ++ show feedback)

gameLoop :: GameState -> IO ()
gameLoop gameState = do
  guess <- getUserInput
  let feedback = checkGuess (secretWord gameState) guess
  let newGameState = updateGameState gameState guess feedback
  displayState newGameState
  if all ((== Igen) . snd) feedback
    then putStrLn "Gratulálok! Kitaláltad a szót!"
    else if length (guesses newGameState) >= maxAttempts newGameState
      then putStrLn "Elfogyott a próbálkozások száma! Vége a játéknak."
      else gameLoop newGameState
