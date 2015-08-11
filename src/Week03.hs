module Week03 where

import Data.Function (on)

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state s i = \x -> if x == s then i else state x

empty :: State
empty = const 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE _ (Val val)       = val
evalE state (Var var)   = state var
evalE state (Op l op r) = (opFn `on` (evalE state)) l r
  where opFn = case op of
                 Plus   -> (+)
                 Minus  -> (-)
                 Times  -> (*)
                 Divide -> div
                 Gt     -> boolToInt (>)
                 Lt     -> boolToInt (<)
                 Ge     -> boolToInt (>=)
                 Le     -> boolToInt (<=)
                 Eql    -> boolToInt (==)
        boolToInt fn x y | fn x y    = 1
                         | otherwise = 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign v e)     = DAssign v e
desugar (Incr v)         = DAssign v (Op (Var v) Plus (Val 1))
desugar (If e t f)       = DIf e (desugar t) (desugar f)
desugar (While e s)      = DWhile e (desugar s)
desugar (For i c m b)    = DSequence (desugar i) (DWhile c (DSequence (desugar b) (desugar m)))
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar Skip             = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign v e)     = extend state v (evalE state e)
evalSimple state (DIf e t f)       = evalSimple state (if evalE state e == 1 then t else f)
evalSimple state (DWhile e s)      = if evalE state e == 1
                                       then let state' = evalSimple state s
                                            in evalSimple state' (DWhile e s)
                                       else state
evalSimple state (DSequence s1 s2) = let state' = evalSimple state s1
                                     in evalSimple state' s2
evalSimple state DSkip             = state

run :: State -> Statement -> State
run state = evalSimple state . desugar

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
