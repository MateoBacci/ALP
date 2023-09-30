module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion l = conv l []

conv :: LamTerm -> [String] -> Term
conv lt ls =
  case lt of 
    LVar s -> 
      case (search s ls) of 
        Just a -> Bound a
        Nothing -> Free (Global s)
    App lt1 lt2 -> (conv lt1 ls) :@: (conv lt2 ls)
    Abs v lt' -> conv lt (ls ++ [v])
  where 
    search :: String -> [String] -> Maybe Int
    search s ls = search' s ls 0
    search' :: String -> [String]  -> Int -> Maybe Int
    search' _ [] _     = Nothing
    search' s (x:xs) n = if s == x then (Just n) else (search' s xs (n+1)) 
     

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp = undefined

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






