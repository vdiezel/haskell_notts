import System.IO
import Data.Char

-- IO indicates actions that interact with the outside world
-- getChar :: IO Char
-- putChar :: Char -> IO ()
-- return :: a -> IO a
--
--
-- Sequencing

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x, y)

--getLine :: IO String
--getLine = do x <- getChar
--             if x == '\n' then
--               return []
--             else
--               do xs <- getLine
--                  return (x:xs)


-- hangman

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]

play :: String -> IO ()
play word =
  do putStr "? "
     guess <- getLine
     if guess == word then
        putStrLn "You got it!"
     else
        do putStrLn (match word guess)
           play word

hangman :: IO ()
hangman = do putStrLn "Think if a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

--- game of nim (my solution)

getInt :: IO Int
getInt = do input <- getChar
            return (read [input] :: Int)

printBoard :: [Int] -> IO ()
printBoard [] = putStrLn "\n"
printBoard (x:xs) = do putStrLn (":" ++ replicate x '*')
                       printBoard xs

updateBoard :: Int -> Int -> [Int] -> [Int]
updateBoard _ _ [] = []
updateBoard 0 newStarsCount (x:xs) = newStarsCount:xs
updateBoard row newStarsCount (x:xs) = x:updateBoard (row - 1) newStarsCount xs

playNim :: [Int] -> IO ()
playNim board = if all (==0) board then
                  putStrLn "You won!"
                else
                  do printBoard board
                     putStrLn "Chose Row"
                     row <- getInt
                     putStrLn ""
                     if (row < 0) || (row > length board) then
                       do
                         putStrLn "Chosen Row Number is out of bounds"
                         playNim board
                     else
                       do putStrLn "Chose Number of stars"
                          number <- getInt
                          putStrLn ""
                          if ((board !! row) < number) || (number < 0) then
                             do putStrLn "Chosen amount of stars invalid"
                                playNim board
                          else
                             playNim (updateBoard row ((board !! row) - number) board)

nim :: IO ()
nim = playNim board
         where board = [5, 4, 3, 2, 1]


-- Grahams Solution

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished b = all (== 0) b

valid :: Board -> Int -> Int -> Bool
valid b row num = b !! (row -1) >= num

move :: Board -> Int -> Int -> Board
move b row num = [adjust r n | (r, n) <- zip [1..5] b]
                  where
                     -- this is a cool way of updating an element by index!
                     adjust r n = if r == row then n-num else n

newline :: IO ()
newline = putChar '\n'

stars :: Int -> String
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (stars num)

putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] = do putRow 1 a
                              putRow 2 b
                              putRow 3 c
                              putRow 4 d
                              putRow 5 e

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

next :: Int -> Int
next 1 = 2
next 2 = 1

gplay :: Board -> Int -> IO ()
gplay board player = do newline
                        putBoard board
                        if finished board then
                          do newline
                             putStr "Player "
                             putStr (show (next player))
                             putStrLn  " wins!"
                        else
                          do newline
                             putStr "Player's "
                             putStr (show player)
                             r <- getDigit "Enter a row number: "
                             n <- getDigit "Enter the number of stars: "
                             if valid board r n then
                               gplay (move board r n) (next player)
                             else
                               do newline
                                  putStrLn "ERROR: Invalid Move!"
                                  gplay board player

gnim :: IO ()
gnim = gplay initial 1
