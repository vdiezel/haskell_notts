-- CourseWork 2 for Graham Huttons Advanced Functional Programmng course!
module Parser where

import System.IO ()
import Control.Monad.State ( evalState, MonadState(state), State, get )
import Data.List
import Data.Maybe
import Debug.Trace

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
                        Assign 'B' (App Sub (Var 'B') (Val 1))
                    ]
                )
             ]

-- Stack-type VM

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
nextLabel = state (\st -> let st' = incCounter st in (st', st'))

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
                              ++ [JUMPZ afterLabel]
                              ++ loopProg
                              ++ [JUMP loopLabel]
                              ++ [LABEL afterLabel]
comp (Seqn []) = return []
comp (Seqn (p:progs)) = do p1 <- comp p
                           ps <- comp (Seqn progs)
                           return $ p1 ++ ps

compileFac :: Code
compileFac = evalState (comp (fac 10)) (-1)

-- | Execution of the code - built as a "state" machine.
-- It should be possible to built it purely recursively
-- without state (I think), but that would yield a quite complex
-- setup (I think)

type Stack = [Int]
type Mem = [(Name, Int)]
type InstructionN = Int

--  ExecutionState
type ExecutionState = (Stack, Mem, InstructionN)
type EST = State ExecutionState

-- | POP name
updateMemory :: Mem -> Char -> Int -> Mem
updateMemory mem name value = if any (\(cName, cValue) -> cName == name) mem then
                [if currName == name then (currName, value) else (currName, currValue) | (currName, currValue) <- mem]
            else
                (name, value) : mem

pop :: Name -> EST Int
pop name =  state (\(stack, mem, inst)
  -> let poppedValue = head stack in (poppedValue , (drop 1 stack, traceShow (updateMemory mem name poppedValue) (updateMemory mem name poppedValue), inst + 1)))

-- | PUSH val
push :: Int -> EST Int
push val = state (\(stack, mem, inst) -> (val, (val:stack, mem, inst + 1)))

-- | assuming for now that the variable is always defined
getMemoryValue :: Mem -> Char -> Int
getMemoryValue mem name = snd (fromJust (find (\(cName, value) -> cName == name) mem))

-- | PUSHV name
pushv :: Name -> EST Int
pushv name = state (\(stack, mem, inst) -> let memVal = getMemoryValue mem name in (memVal, (memVal:stack, mem, inst + 1)))

-- || JUMP label
compareByLabel :: Inst -> Int -> Bool
compareByLabel (LABEL n) label = n == label
compareByLabel _ label = False

getLabelIndex :: Code -> Label -> Int
getLabelIndex code label = snd (fromJust (find (\(inst, idx) -> compareByLabel inst label) (zip code [0 .. length code - 1])))

jump :: Code -> Label -> EST Int
jump code label = state (\(stack, mem, inst) -> (label, (stack, mem, getLabelIndex code label + 1)))

-- || JUMPZ label
jumpz :: Code -> Label -> EST Int
jumpz code label = state (\(stack, mem, inst)
  -> let poppedValue = head stack in  (poppedValue, (drop 1 stack, mem, if poppedValue == 0 then getLabelIndex code label + 1 else inst + 1)))

-- | LABEL label doesn't do anything, we will use a "skip"
skip :: EST Int
skip = state (\(stack, mem, inst) -> (inst + 1, (stack, mem, inst + 1)))

-- DO Op
applyOperator :: Op -> Int -> Int -> Int
applyOperator Add a b = a + b
applyOperator Sub a b = a - b
applyOperator Mul a b = a * b
applyOperator Div a b = a `div` b -- app will crash when deviding by zero! (intended behaviour)

doOp :: Op -> EST Int
doOp op = state (\(stack, mem, inst)
  -> let right = head stack -- right value comes first from the stack! (as pushed last)
         left = stack !! 1
         res = applyOperator op left right
     in (res, (res:drop 2 stack, mem, inst + 1))
  )

executeInstruction :: Inst -> Code -> EST Int
executeInstruction (PUSH val) code = push val
executeInstruction (PUSHV name) code = pushv name
executeInstruction (POP name) code = pop name
executeInstruction (LABEL label) code = skip
executeInstruction (DO op) code = doOp op
executeInstruction (JUMP label) code = jump code label
executeInstruction (JUMPZ label) code = jumpz code label

execute :: Code -> EST ExecutionState
execute code = do (stack, mem, inst) <- get
                  if inst == length code then
                    return (stack, mem, inst) -- we are done
                  else
                    do out <- executeInstruction (traceShow (code !! inst) (code !! inst)) code
                       execute code
main :: IO ()
main = print $ evalState (execute compileFac) ([], [], 0)
