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
lookfor :: Variable -> State -> Int
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
stepComm c s = undefined 
{-case c of
    Skip -> 
    Let v i ->
    Seq c1 c2 ->
    IfThenElse b c1 c2 ->
    Repeat c1 b -> -}

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp = undefined
