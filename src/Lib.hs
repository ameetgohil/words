module Lib
    ( outputGrid
    , formatGrid
    , findWord
    , findWords
    , findWordInLine
    , getLines
    , skew
    ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

outputGrid :: Grid -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

getLines :: Grid -> [String]
getLines grid =
  let horizontal = grid
      vertical = transpose grid
      diagonal1 = diagonalize grid
      diagonal2 = diagonalize (map reverse grid)
      lines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
  in lines ++ (map reverse lines)

diagonalize :: Grid -> Grid
diagonalize = (transpose . skew)

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent line = '_' : line

findWord :: Grid -> String -> Maybe String
findWord grid word =
  let lines = getLines grid
      found = or $ map (findWordInLine word) lines
  in if found then Just word else Nothing

--findWords :: Grid -> [String] -> [Bool]
findWords grid words = catMaybes $ map (findWord grid) words

findWordInLine :: String -> String -> Bool
findWordInLine = isInfixOf

