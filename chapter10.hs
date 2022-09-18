import System.IO

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

