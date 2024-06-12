module Main (main) where

import Lib (selectDifficulty, difficultyFile, loadWords, initGame, gameLoop)
import System.IO (hSetEncoding, utf8, stdout, stdin)


main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  difficulty <- selectDifficulty
  animals <- loadWords (difficultyFile difficulty)
  gameState <- initGame animals
  gameLoop gameState animals
