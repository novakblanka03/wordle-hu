module Lib (selectDifficulty, difficultyFile, loadWords, initGame, gameLoop) where

import System.Random (randomRIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (toUpper)
import System.IO (hSetEncoding, utf8, stdin)

-- define data types for feedback and difficulty (enumerations)
data Feedback = Igen | Majdnem | Nem deriving (Show, Eq)
data Difficulty = Kezdo | Kozepes | Halado | Profi | Nehez deriving (Show, Eq)

-- define game state, and its constructor
data GameState = GameState { secretWord :: T.Text, guesses :: [(T.Text, [(T.Text, Feedback)])], maxAttempts :: Int }


-- show feedback with colours (not using this in the current version of the game)
feedbackShow :: Feedback -> Char
feedbackShow feedback =
  case feedback of
    Nem -> '⬛'
    Igen -> '🟩'
    Majdnem -> '🟨'

-- select difficulty level
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

-- get file path based on difficulty
difficultyFile :: Difficulty -> FilePath
difficultyFile Kezdo  = "src/kezdo.txt"
difficultyFile Kozepes = "src/kozepes.txt"
difficultyFile Halado = "src/halado.txt"
difficultyFile Profi  = "src/profi.txt"
difficultyFile Nehez  = "src/nehez.txt"

-- load words from file
loadWords :: FilePath -> IO [T.Text]
loadWords path = do
    hSetEncoding stdin utf8
    content <- TIO.readFile path
    return (T.lines content)

-- initialize the game state with a random secret word from file
initGame :: [T.Text] -> IO GameState
initGame animals = do
  index <- randomRIO (0, length animals - 1)
  let secret = animals !! index
--   putStrLn (T.unpack secret)
  return $ GameState secret [] 6

-- get user input from console (not working correctly in case of hungarian characters)
getUserInput :: IO T.Text
getUserInput = do
  putStrLn "\nÍrd be a tipped: "
  TIO.getLine

-- compare secret word with guess and give feedback
checkGuess :: T.Text -> T.Text -> [(T.Text, Feedback)]
checkGuess secret guess = map check (T.zip secret (T.map toUpper guess))
  where
    check (s, g)
      | s == g = (T.singleton g, Igen)
      | g `T.elem` secret = (T.singleton g, Majdnem)
      | otherwise = (T.singleton g, Nem)

-- check if the guess is valid
validGuess :: T.Text -> [T.Text] -> Bool
validGuess guess animals = T.map toUpper guess `elem` animals

-- update the game state with the new guess and feedback
updateGameState :: GameState -> T.Text -> [(T.Text, Feedback)] -> GameState
updateGameState gameState guess feedback =
  gameState { guesses = (guess, feedback) : guesses gameState }

-- display the current state of the game
displayState :: GameState -> IO ()
displayState gameState = do
  putStrLn "Eddigi tippek:"
  mapM_ printGuess (guesses gameState)
  where
    printGuess (guess, feedback) = putStrLn (T.unpack guess ++ " " ++ show feedback)

-- main game logic
gameLoop :: GameState -> [T.Text] -> IO ()
gameLoop gameState animals = do
  guess <- getUserInput
  print (T.unpack guess)
  if not (validGuess guess animals)
    then do
      putStrLn "Érvénytelen tipp! Kérjük próbáld újra."
      gameLoop gameState animals
    else do
      let feedback = checkGuess (secretWord gameState) guess
      let newGameState = updateGameState gameState guess feedback
      displayState newGameState
      if all ((== Igen) . snd) feedback
        then putStrLn "Gratulálok! Kitaláltad a szót!"
        else if length (guesses newGameState) >= maxAttempts newGameState
          then putStrLn "Elfogyott a próbálkozások száma! Vége a játéknak."
          else gameLoop newGameState animals