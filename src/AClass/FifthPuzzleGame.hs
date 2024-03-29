{-# LANGUAGE ScopedTypeVariables #-}
module AClass.FifthPuzzleGame where

import Data.Array
import System.Random

type Puzzle = Array (Int,Int) Int
runMain :: IO()
runMain = do
    putStrLn "Please enter the difficulty level:0,1 or 2"
    userInput <-getLine
    let diffLevel = read userInput :: Int
    if userInput == "" || any (\c -> c <'0' || c > '9') userInput || diffLevel > 2 || diffLevel < 0
        then putStrLn "That is not a valid difficulty level." >> runMain
        else shufflePuzzle ([10,50,100] !! diffLevel) solvedPuzzle >>= gameLoop

gameLoop :: Puzzle -> IO()
gameLoop puzzle
        | puzzle == solvedPuzzle = putStrLn "You solved puzzle!">> printPuzzle puzzle
        | otherwise = do
            printPuzzle puzzle
            putStrLn "Please enter number to above"
            userInput <- getLine
            if any (\c -> c < '0' || c > '9') userInput
                then putStrLn "That is not valid number" >> gameLoop puzzle
                else let move = read userInput in
                if move `notElem` validMoves puzzle
                then putStrLn "this move is not avaiable." >> gameLoop puzzle
                else gameLoop (applyMove move puzzle)
validMoves :: Puzzle -> [Int]
validMoves puzzle = [puzzle ! (row', column') |
   row' <- [rowEmpty - 1 .. rowEmpty + 1],
   row' < 4,
   row' >= 0,
   column' <- [columnEmpty - 1 .. columnEmpty + 1],
   column' < 4,
   column' >= 0,
   (row' == rowEmpty) /= (column' == columnEmpty)]
  where (rowEmpty,columnEmpty) = findIndexOfNumber 16 puzzle
findIndexOfNumber :: (Ix p, Eq e) => e -> Array p e -> p
findIndexOfNumber number puzzle = case filter (\idx -> number == puzzle ! idx )  (indices puzzle) of
                                        [idx] -> idx
                                        _     -> error "Bug: number not in puzzle"
applyMove :: Int -> Puzzle -> Puzzle 
applyMove numberToMove puzzle = puzzle // [(indexToMove,16),(emptyIndex,numberToMove)]
    where indexToMove = findIndexOfNumber numberToMove puzzle
          emptyIndex = findIndexOfNumber 16 puzzle

solvedPuzzle = listArray ((0,0),(3,3)) [1..16]

shufflePuzzle :: Int -> Puzzle -> IO Puzzle
shufflePuzzle 0 puzzle = return puzzle
shufflePuzzle numberOfShuffels puzzle = do
    let moves = validMoves puzzle
    randomIndex <- randomRIO (0,length moves -1)
    let move = moves !! randomIndex
    shufflePuzzle (numberOfShuffels-1) (applyMove move puzzle)

printPuzzle puzzle = do
    putStrLn "+--+--+--+--+"
    putStrLn $ "|" ++ formatCell (0, 0) ++ "|" ++ formatCell (0, 1) ++ "|" ++ formatCell (0, 2) ++ "|" ++ formatCell (0, 3) ++ "|"
    putStrLn "+--+--+--+--+"
    putStrLn $ "|" ++ formatCell (1, 0) ++ "|" ++ formatCell (1, 1) ++ "|" ++ formatCell (1, 2) ++ "|" ++ formatCell (1, 3) ++ "|"
    putStrLn "+--+--+--+--+"
    putStrLn $ "|" ++ formatCell (2, 0) ++ "|" ++ formatCell (2, 1) ++ "|" ++ formatCell (2, 2) ++ "|" ++ formatCell (2, 3) ++ "|"
    putStrLn "+--+--+--+--+"
    putStrLn $ "|" ++ formatCell (3, 0) ++ "|" ++ formatCell (3, 1) ++ "|" ++ formatCell (3, 2) ++ "|" ++ formatCell (3, 3) ++ "|"
    putStrLn "+--+--+--+--+"
    where formatCell idx
              | i == 16 = "  "
              | i > 9 = show i
              | otherwise = " " ++ show i
              where i = puzzle ! idx