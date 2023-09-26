module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple hiding (fst, snd)

-- Estados 
type State = (M.Map Variable Int, String)

-- Agrega una traza dada al estado
addTrace :: String -> State -> State
addTrace t s = (fst s, (snd s) ++ t)

-- Estado nulo
initState :: State
initState = (M.empty, "")

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case M.lookup v (fst s) of
                Just a -> Right a
                Nothing -> Left UndefVar

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update x n s = let s' = M.insert x n (fst s)
               in (s', snd s)

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm c s =
  case c of
    Skip -> Right (Skip :!: s)
    Let v i ->
      case evalExp i s of
        Left error -> Left error
        Right (n :!: s') ->
          let s'' = addTrace ("Let " ++ v ++ " = " ++ (show n) ++ " - ") s'
          in Right (Skip :!: update v n s'')
    Seq c1 c2 ->
      case c1 of
        Skip -> Right (c2 :!: s)
        _ -> case stepComm c1 s of
              Left error -> Left error
              Right (c1' :!: s') -> Right ((Seq c1' c2) :!: s')
    IfThenElse b c1 c2 -> 
      case evalExp b s of
        Left error -> Left error
        Right (b' :!: s') -> if b' then Right (c1 :!: s') else Right (c2 :!: s')
    Repeat c1 b -> Right ((Seq c1 (IfThenElse b Skip (Repeat c1 b))) :!: s)

-- Evalua una expresion
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp e s =
  case e of
    Const n -> Right (n :!: s)
    Var v -> case lookfor v s of
               Right n -> Right (n :!: s)
               _ -> Left UndefVar
    UMinus n -> case evalExp n s of
                  Right (n' :!: s') -> Right ((-n') :!: s')
                  Left error -> Left error
    Plus a b -> fun a b (+)
    Minus a b -> fun a b (-)
    Times a b -> fun a b (*)
    Div a b -> case evalExp a s of
                Right (a' :!: s') -> 
                  case evalExp b s' of
                    Right (b' :!: s'') -> if b' == 0
                                          then Left DivByZero
                                          else Right ((div a' b') :!: s'')
                    Left error -> Left error
                Left error -> Left error
    ESeq e1 e2 -> case evalExp e1 s of
                    Right (n1 :!: s') -> evalExp e2 s'
                    Left error -> Left error
    EAssgn v n -> case evalExp n s of
                    Right (n' :!: s') -> Right (n' :!: update v n' s)
                    Left error -> Left error
    BTrue -> Right (True :!: s)
    BFalse -> Right (False :!: s)
    Lt a b -> fun a b (<)
    Gt a b -> fun a b (>)
    And a b -> fun a b (&&)
    Or a b -> fun a b (||)
    Not a -> case evalExp a s of
               Right (a' :!: s') -> Right ((not a') :!: s')
               Left error -> Left error
    Eq a b -> fun a b (==)
    NEq a b -> fun a b (/=)
    where
    fun :: Exp a -> Exp a -> (a -> a -> b) -> Either Error (Pair b State)
    fun = \a b op -> 
      case evalExp a s of
        Right (a' :!: s') -> 
          case evalExp b s' of
            Right (b' :!: s'') -> Right ((op a' b') :!: s'')
            Left error -> Left error
        Left error -> Left error

