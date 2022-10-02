module Parser where

import System.IO ()
import Control.Monad.State ( evalState, MonadState(state), State )

data Prog = Assign Name Expr
    | If Expr Prog Prog
    | While Expr Prog
    | Seqn [Prog]

data Expr = Val Int
    | Var Name
    | App Op Expr Expr

type Name = Char
data Op   = Add | Sub | Mul | Div
            deriving (Show)

fac :: Int -> Prog
fac n = Seqn [
                    Assign 'A' (Val 1),
                    Assign 'B' (Val n),
                    While (Var 'B') (
                        Seqn [
                            Assign 'A' (App Mul (Var 'A') (Var 'B')),
                            Assign 'B' (App Sub (Var 'A') (Var 'B'))
                        ]
                    )
                 ]

-- Stack-type VM

type Stack = [Int]
type Mem = [(Name, Int)]
type Code = [Inst]
type Label = Int

-- Our 7 Instructions

data Inst = PUSH Int -- pushes an Int value to the top of the stack
          | PUSHV Name -- takes the value of the variable "Name" and pushes it
          | POP Name -- pops value from the stack and puts it into "Name"
          | DO Op -- removes top two values from the stack, does the operation and put them back onto the stack
          | JUMP Label -- unconditional jump
          | JUMPZ Label -- pops from the stack, if value is 0, jump to label
          | LABEL Label -- labels location in the code
          deriving (Show)

-- LabelGenerator - taken from the docs of the State Monad
type LabelCounter = Int
type ST = State LabelCounter
incCounter :: LabelCounter -> LabelCounter
incCounter c = c + 1

nextLabel :: ST Int
nextLabel =  state (\st -> let st' = incCounter st in (st', st'))

-- Compile Logic

compExpr :: Expr -> Code
compExpr (Val n) = [PUSH n]
compExpr (Var name) = [PUSHV name]
compExpr (App op l r) = compExpr l ++ compExpr r ++ [DO op]

comp :: Prog -> ST Code
comp (Assign name expr)     = return (compExpr expr ++ [POP name])
comp (If expr pTrue pFalse) = do falseLabel <- nextLabel
                                 finalLabel <- nextLabel
                                 trueSide   <- comp pTrue
                                 falseSide  <- comp pFalse
                                 return $
                                   compExpr expr
                                   ++ [JUMPZ falseLabel]
                                   ++ trueSide
                                   ++ [JUMP finalLabel]
                                   ++ [LABEL falseLabel]
                                   ++ falseSide
                                   ++ [LABEL finalLabel]
comp (While expr prog) = do loopLabel  <- nextLabel
                            afterLabel <- nextLabel
                            loopProg   <- comp prog
                            return $
                              [LABEL loopLabel]
                              ++ compExpr expr
                              ++ [JUMPZ loopLabel]
                              ++ loopProg
                              ++ [JUMP loopLabel]
                              ++ [LABEL afterLabel]
comp (Seqn []) = return []
comp (Seqn (p:progs)) = do p1 <- comp p
                           ps <- comp (Seqn progs)
                           return $ p1 ++ ps


main = print $ evalState (comp (fac 10)) (-1)
