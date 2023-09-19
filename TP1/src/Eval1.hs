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
lookfor v s = case M.lookup v s of
                Just a -> a
                Nothing -> 0 -- lookup en realidad devuelve Maybe, no Int. Ver eso. 

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
                Let v i -> let (n :!: s') = evalExp i s
                           in (Skip :!: update v n s')
                Seq c1 c2 -> case c1 of
                              Skip -> (c2 :!: s)
                              _ -> let (c1' :!: s') = stepComm c1 s
                                   in ((Seq c1' c2) :!: s')
                IfThenElse b c1 c2 -> let (b' :!: s') = evalExp b s
                                      in if b' then (c1 :!: s') else (c2 :!: s')
                Repeat c1 b -> ((Seq c1 (IfThenElse b Skip (Repeat c1 b))) :!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
evalExp e s = case e of 
  Const n -> (n :!: s)
  Var v -> ((lookfor v s) :!: s)
  UMinus n -> let (n' :!: s') = evalExp n s
              in ((-n') :!: s')
  Plus a b -> let (a' :!: s') = evalExp a s
                  (b' :!: s'') = evalExp b s'
              in ((a' + b') :!: s'')
  Minus a b -> let (a' :!: s') = evalExp a s
                   (b' :!: s'') = evalExp b s'
               in ((a' - b') :!: s'')
  Times a b -> let (a' :!: s') = evalExp a s
                   (b' :!: s'') = evalExp b s'
               in ((a' * b') :!: s'')
  Div a b -> let (a' :!: s') = evalExp a s
                 (b' :!: s'') = evalExp b s'
             in ((div a' b') :!: s'')
  ESeq e1 e2 -> let (n1 :!: s') = evalExp e1 s 
                    (n2 :!: s'') = evalExp e2 s'
                in (n2 :!: s'')
  EAssgn v n -> let (n' :!: s') = evalExp n s 
                in (n' :!: (update v n' s))
  BTrue -> (True :!: s)
  BFalse -> (False :!: s)
  Lt a b -> let (a' :!: s') = evalExp a s
                (b' :!: s'') = evalExp b s'
            in ((a' < b') :!: s'')
  Gt a b -> let (a' :!: s') = evalExp a s
                (b' :!: s'') = evalExp b s'
            in ((a' > b') :!: s'')
  And a b -> let (a' :!: s') = evalExp a s
                 (b' :!: s'') = evalExp b s'
             in ((a' && b') :!: s'')
  Or a b -> let (a' :!: s') = evalExp a s
                (b' :!: s'') = evalExp b s'
            in ((a' || b') :!: s'')
  Not a -> let (a' :!: s') = evalExp a s
           in ((not a') :!: s')
  Eq a b -> let (a' :!: s') = evalExp a s
                (b' :!: s'') = evalExp b s'
            in ((a' == b') :!: s'')
  NEq a b -> let (a' :!: s') = evalExp a s
                 (b' :!: s'') = evalExp b s'
             in ((a' /= b') :!: s'')

