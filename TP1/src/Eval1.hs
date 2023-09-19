module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar definicion
lookfor :: Variable -> State -> Maybe Int
lookfor v s = M.lookup v s -- lookup en realidad devuelve Maybe, no Int. Ver eso. 

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update v i m = M.insert v i m

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm c s = case c of
                Skip -> (Skip :!: s)
                Let v i -> let (n :!: s') = evalExp i s in
                  (Skip :!: update v n s')
                Seq c1 c2 -> case c1 of
                              Skip -> (c2 :!: s)
                              _ -> let (c1' :!: s') = stepComm c1 s in 
                                    ((Seq c1' c2) :!: s')
                IfThenElse b c1 c2 -> let (b' :!: s') = evalExp b s in
                                        if b' then (c1 :!: s') else (c2 :!: s')
                Repeat c1 b -> ((Seq c1 (IfThenElse b Skip (Repeat c1 b))) :!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
evalExp e s = case e of 
  Const n -> (n :!: s)
  Var v -> ((lookfor v s) :!: s)
  UMinus n -> ((-n) :!: s)
  Plus a b -> ((a + b) :!: s)
  Minus a b -> ((a - b) :!: s)
  Times a b -> ((a * b) :!: s)
  Div a b -> ((div a b) :!: s)
  ESeq e1 e2 -> let (n1 :!: s') = evalExp e1 s 
                    (n2 :!: s'') = evaleExp e2 s'
                in (n2 :!: s'')
  EAssgn v n -> (n :!: (update v n s))
  BTrue -> (True :!: s)
  BFalse -> (False :!: s)
  Lt a b -> ((a < b) :!: s)
  Gt a b -> ((a > b) :!: s)
  And a b -> ((a && b) :!: s)
  Or a b -> ((a || b) :!: s)
  Not a -> ((!a) :!: s)
  Eq a b -> ((a == b) :!: s)
  NEq a b ->  ((a /= b) :!: s)

{-
Const :: Int → Exp Int
Var :: Variable → Exp Int
UMinus :: Exp Int → Exp Int
Plus :: Exp Int → Exp Int → Exp Int
Minus :: Exp Int → Exp Int → Exp Int
Times :: Exp Int → Exp Int → Exp Int
Div :: Exp Int → Exp Int → Exp Int
BTrue :: Exp Bool
BFalse :: Exp Bool
Lt :: Exp Int → Exp Int → Exp Bool
Gt :: Exp Int → Exp Int → Exp Bool
And :: Exp Bool → Exp Bool → Exp Bool
Or :: Exp Bool → Exp Bool → Exp Bool
Not :: Exp Bool → Exp Bool
Eq :: Exp Int → Exp Int → Exp Bool
NEq :: Exp Int → Exp Int → Exp Bool
-}
