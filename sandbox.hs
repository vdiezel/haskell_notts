-- | given an array A or strings S and given an Array B of chars C, remove
-- all chars that are in B from every S of A

isTaken :: [Char] -> Char -> Bool
isTaken chars char = char `elem` chars

removeSingleChars :: [[Char]] -> [Char] -> [[Char]]
-- removeSingleChars strings (char:chars) = removeSingleChars (map (T.replace char "") strings) chars
-- removeSingleChars strings [] = strings
removeSingleChars choices taken = map (filter (not . isTaken taken)) choices

