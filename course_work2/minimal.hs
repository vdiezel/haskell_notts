import Control.Monad.State ( MonadState(state), evalState, State )

-- LabelGenerator - taken from the docs of the State Monad
type LabelCounter = Int
type ST = State LabelCounter
incCounter :: LabelCounter -> LabelCounter
incCounter c = c + 1

nextLabel :: ST Int
nextLabel =  state (\st -> let st' = incCounter st in (st', st'))

-- Exercise: define a function like this:
f :: Int -> ST [Int]
f 0 = return ([])
f n = do label1  <- nextLabel
         label2  <- nextLabel
         res <- f (n - 1)
         return ([label1, label2] ++ res)

-- main :: IO()
main :: IO () 
main = do print (evalState (f 10) 0)

