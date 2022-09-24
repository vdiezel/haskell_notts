-- connect 4 with game trees && minimax algorithms
rows = 6
cols = 7
win = 4
depth = 3 -- search depth

type Board = [Row]
type Row = [Player]
data Player = O | B | X
              deriving (Ord, Eq, Show)
              -- Ord orders according to the order of definition of the
              -- constructor O <= B <= X (also min and max work!)

-- minimax stratefy for the computer player
-- 1. produce gametree to a specified depth
-- 2. Label each leaf with the winner, or B if not over or draw
-- 3. Then, work up the tree
--   If O to play, take the minimum of children (O < B < X)
--   If X to play, take the maximum of children
-- 4. Best move is one with the same label as the root (the player who's turn it is)


