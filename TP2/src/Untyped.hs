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
conv (App lt1 lt2) ls = (conv lt1 ls) :@: (conv lt2 ls)
conv (Abs v lt')   ls = Lam (conv lt' (v:ls))
conv (LVar s)      ls = case (search s ls) of 
                          Just a -> Bound a
                          Nothing -> Free (Global s)  
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
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)
    
eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii)   (_, lEnv)   = lEnv !! ii
eval' (Free n)     (gEnv, _)   = case lookup n gEnv of
                                    Just v -> v
                                    Nothing -> VNeutral (NFree n)
eval' (t1 :@: t2)      e        = vapp (eval' t1 e) (eval' t2 e)
eval' (Lam t)      (gEnv, lEnv) = VLam (\x -> eval' t (gEnv, x:lEnv))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote v = quote' v 0

quote' :: Value -> Int -> Term
quote' (VLam f)                     i = Lam (quote' (f (VNeutral(NFree (Quote i)))) (i+1))
quote' (VNeutral (NFree (Quote k))) i = Bound (i - k - 1)
quote' (VNeutral (NFree nom))       i = Free nom
quote' (VNeutral (NApp neu val))    i = (quote' (VNeutral neu) i) :@: (quote' val i)
