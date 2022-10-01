-- My first attempt at writing Connect4 with Haskell
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List
import Data.Char
import System.IO
import Data.Map (valid)
import Debug.Trace
import Data.Maybe

rows :: Int
rows = 5
cols :: Int
cols = 5
win :: Int
win = 4
maxDepth :: Int
maxDepth = 2 -- search depth

type Board = [Row]
type Row = [Player]
type Depth = Int
data Player = O | B | X
              deriving (Ord, Eq, Show)
              -- constructor O <= B <= X (also min and max work!)

data GameTree a = Node a [GameTree a]
                  deriving (Show)

initialBoard :: Board
initialBoard = replicate rows (replicate cols B)

getRows :: Board -> [Row]
getRows board = [ drop x (take y row) | (x, y) <- zip [0..(cols - win)] [win..cols], row <- board]

getColumns :: Board -> [Row]
getColumns board = [ drop x (take y row) | (x, y) <- zip [0..(rows - win)] [win..rows], row <- transpose board]

generateDia :: (Int -> Int -> Int) -> Board -> (Int, Int) -> Row
generateDia moveFn board (row, col) = [ (board !! calcRow offset) !! (col + offset) | offset <- [0..(win - 1)]]
                                      where calcRow offset = moveFn row offset

colRange :: [Int]
colRange = [0..(cols - win)]
getStartPoints :: [Int] -> [(Int, Int)]
getStartPoints rowRange = [(row, col) | row <- rowRange, col <- colRange]

diagonalsInc :: Board -> [Row]
diagonalsInc board = map (generateDia (-) board) startPoints
                     where startPoints = getStartPoints [(win - 1)..(rows - 1)]

diagonalsDec :: Board -> [Row]
diagonalsDec board = map (generateDia (+) board) startPoints
                     where startPoints = getStartPoints [0..(rows - win)]

getDiagonals :: Board -> [Row]
getDiagonals board = diagonalsInc board ++ diagonalsDec board

printBoard :: Board -> IO()
printBoard [] = do print [0..cols - 1]
printBoard (row:board) = do print row
                            printBoard board

validMove :: Board -> Int -> Bool
validMove board col = head board !! col == B

playerWinsRow :: Player -> Row -> Bool
playerWinsRow player = all (== player)

playerWins :: Player -> Board -> Bool
playerWins player board = any (playerWinsRow player) (getRows board)
                          || any (playerWinsRow player) (getColumns board)
                          || any (playerWinsRow player) (getDiagonals board)

playerXWins :: Board -> Bool
playerXWins =  playerWins X

playerOWins :: Board -> Bool
playerOWins = playerWins O

isFullBoard :: Board -> Bool
isFullBoard board = B `notElem` head board

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do newline
                           putStrLn "ERROR: Invalid Digit!"
                           getDigit prompt

getFirstEmptyRow :: Board -> Int -> Int
getFirstEmptyRow board col = snd (last (takeWhile (\(row, rowIdx) -> row !! col == B) (zip board [0..])))

updateRow :: Row -> Player -> Int -> Row
updateRow row player colIdx = zipWith (\ col currColIdx -> (if currColIdx == colIdx then player else col)) row [0..cols - 1]

addToFirstEmptySpot :: Board -> Player -> Int -> Int -> Board
addToFirstEmptySpot board player colIdx rowIdx = zipWith (\ row currRowIdx
  -> (if currRowIdx == rowIdx then updateRow row player colIdx else row)) board [0..rows - 1]

addPlayerChoice :: Board -> Player -> Int -> Board
addPlayerChoice board player col = addToFirstEmptySpot board player col row
                                   where row = getFirstEmptyRow board col

getTurnFromDepth :: Depth -> Player
getTurnFromDepth depth | even depth = O
                       | otherwise = X

validMoves :: Board -> [Int]
validMoves board = filter (validMove board) [0 .. cols - 1]

computeBranch :: Board -> Int -> Depth -> GameTree (Board, Int, Depth, Player)
computeBranch board playedColumn depth
  | playerXWins board = Node (board, playedColumn, depth, X) []
  | playerOWins board = Node (board, playedColumn, depth, O) []
  | depth == maxDepth || isFullBoard board = Node (board, playedColumn, depth, B) []
  | otherwise = Node (board, playedColumn, depth, B) (map (
    \ colIdx
       -> computeBranch (addColumnChoice colIdx) colIdx (depth + 1)
    ) (validMoves board))
    where addColumnChoice colIdx = addPlayerChoice board (getTurnFromDepth (depth + 1)) colIdx

computeGameTree :: Board -> [GameTree (Board, Int, Depth, Player)]
computeGameTree board = map (\colIdx -> computeBranch board colIdx 0) (validMoves board)

-- minimax stratefy for the computer player
-- 1. produce gametree to a specified depth
-- 2. Label each leaf with the winner, or B if not over or draw
-- 3. Then, work up the tree
--   If O to play, take the minimum of children (O < B < X)
--   If X to play, take the maximum of children
-- 4. Best move is one with the same label as the root (the player who's turn it is)

minimax :: GameTree (Board, Int, Depth, Player) -> GameTree (Player, Int)
minimax (Node (board, colIdx, depth, player) []) = Node (player, colIdx) []
minimax (Node (board, colIdx, depth, player) nodes)
  | depth == 0 = Node (player, colIdx) (map minimax nodes)
  | depth > 0  = Node (
      if odd depth then
         minimum (map ((\(Node (currPlayer, colIdx) []) -> currPlayer) . minimax) nodes)
      else
         maximum (map ((\(Node (currPlayer, colIdx) []) -> currPlayer) . minimax) nodes),
      colIdx
      ) []

choseBestTurn :: GameTree (Player, Int) -> Int
choseBestTurn (Node (player, colIdx) nodes)
  | isJust foundWin = snd (fromJust foundWin)
  | isJust foundTie = snd (fromJust foundTie)
  | otherwise = 0
  where
      foundWin = find (\ (currPlayer, currColIdx) -> currPlayer == X) unwrappedNodes
      foundTie = find (\ (currPlayer, currColIdx) -> currPlayer == B) unwrappedNodes
      unwrappedNodes = map (\(Node (currPlayer, colIdx) []) -> (currPlayer, colIdx)) nodes

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer X = O

play :: Board -> Player -> IO()
play board player = do putStrLn ("Players " ++ show player ++ " turn.")
                       printBoard board
                       if playerXWins board then
                         putStrLn "Computer wins!"
                       else if playerOWins board then
                         putStrLn "Player wins!"
                       else if isFullBoard board then
                         putStrLn "Draw!"
                       else
                         if player == O then
                           do column <- getDigit "Enter the column for your turn"
                              if not (validMove board column) then
                                do putStrLn "The column you have chosen is full"
                                   play board player
                              else
                                play (addPlayerChoice board player column) (nextPlayer player)
                         else
                           do putStrLn "The AI is thinking..."
                              -- print (head (computeGameTree board))
                              -- print (head (map minimax (computeGameTree board)))
                              play (addPlayerChoice board player bestTurn) (nextPlayer player)
                              where
                                bestTurn = choseBestTurn (head (map minimax (computeGameTree board)))

main :: IO()
main = play initialBoard O
