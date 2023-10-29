module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b LUnit           = Unit
conversion' b (LVar n      )  = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u    )  = conversion' b t :@: conversion' b u
conversion' b (LAbs n t  u )  = Lam t (conversion' (n : b) u)
conversion' b (LLet x e1 e2)  = Let (conversion' b e1) (conversion' (x : b) e2)
conversion' b (LPair e1 e2 )  = Pair (conversion' b e1) (conversion' b e2)  
conversion' b (LFst p      )  = Fst (conversion' b p)  
conversion' b (LSnd p      )  = Snd (conversion' b p)  
conversion' b LZero           = Zero
conversion' b (LSuc n      )  = Suc (conversion' b n)
conversion' b (LRec t1 t2 t3) = Rec (conversion' b t1) (conversion' b t2) (conversion' b t3)


-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t Unit                  = Unit
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let e1 e2)           = Let (sub i t e1) (sub i t e2)
sub i t (Pair e1 e2)          = Pair (sub i t e1) (sub i t e2)
sub i t (Fst p)               = Fst (sub i t p)
sub i t (Snd p)               = Snd (sub i t p)
sub i t Zero                  = Zero
sub i t (Suc n)               = Suc (sub i t n)
sub i t (Rec t1 t2 t3)        = Rec (sub i t t1) (sub i t t2) (sub i t t3)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval e Unit                   = VUnit
eval _ (Lam      t   u      ) = VLam t u
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"
eval e (Let e1 e2           ) = let v = eval e e1 in eval e (sub 0 (quote v) e2)
eval e (Pair e1 e2          ) = VPair (eval e e1) (eval e e2)
eval e (Fst p               ) = case eval e p of
                                  (VPair a b) -> a
                                  err         -> err
eval e (Snd p               ) = case eval e p of
                                  (VPair a b) -> b
                                  err         -> err
eval e Zero                   = VNum NZero
eval e (Suc n               ) = VNum (NSuc (numEval n))
eval e (Rec t1 t2 Zero      ) = eval e t1
eval e (Rec t1 t2 (Suc t3)  ) = eval e ((t2 :@: (Rec t1 t2 t3)) :@: t3)
eval e (Rec t1 t2 t3        ) = eval e (Rec t1 t2 (quote (eval e t3)))


numEval :: Term -> NumVal
numEval Zero    = NZero
numEval (Suc n) = NSuc (numEval n)  
-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VUnit)         = Unit
quote (VLam t f)      = Lam t f
quote (VPair a b)     = Pair (quote a) (quote b)
quote (VNum NZero)    = Zero
quote (VNum (NSuc n)) = Suc (quote (VNum n))

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e Unit = ret UnitT
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let a b) = case (infer' c e a) of
                            Right t  -> infer' (t : c) e b
                            err      -> err
infer' c e (Pair a b) = case (infer' c e a) of
                             Right t1 -> case (infer' c e b) of
                                              Right t2 -> ret (PairT t1 t2)
                                              err -> err
                             err      -> err
infer' c e (Fst u) = case (infer' c e u) of
                          Right (PairT t1 t2) -> ret t1
                          Right t             -> matchError (PairT t t) t
                          err                 -> err
infer' c e (Snd u) = case (infer' c e u) of
                          Right (PairT t1 t2) -> ret t2
                          Right t             -> matchError (PairT t t) t
                          err                 -> err
infer' c e Zero    = ret NatT
infer' c e (Suc n) =
  case infer' c e n of
    Right NatT -> ret NatT
    Right t    -> matchError NatT t
    error      -> error 
infer' c e (Rec t1 t2 t3) = 
  case infer' c e t1 of
    Right t1' ->
      case infer' c e t2 of
        Right (FunT t1' (FunT NatT t2')) -> 
          case t1' == t2' of
            False -> matchError t1' t2'
            _  -> case infer' c e t3 of
                        Right NatT -> ret t1'
                        Right t    -> matchError NatT t
                        error      -> error 
          
        Right t            -> matchError (FunT t1' (FunT NatT t1')) t
        error              -> error
    error     -> error
----------------------------------
