module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm c = case c of
                Skip -> return Skip
                Let v i -> do n <- evalExp i
                              update v n
                              return Skip
                Seq c1 c2 -> case c1 of
                              Skip -> return c2
                              _ -> do c1' <- stepComm c1
                                      return (Seq c1' c2)
                IfThenElse b c1 c2 -> do b' <- evalExp b 
                                         if b' then return c1
                                               else return c2
                While b c1 -> do b' <- evalExp b
                                 if b' then return $ Seq c1 $ While b c1  
                                       else return $ Skip
                                       
-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp e = case e of 
  Const n -> return n
  Var v -> do val <- lookfor v
              return val
  UMinus n -> do n' <- evalExp n
                 return $ -n'
  Plus a b -> fun a b (+)
  Minus a b -> fun a b (-)
  Times a b -> fun a b (*)
  Div a b -> fun a b div
  ESeq e1 e2 -> do evalExp e1 
                   n2 <- evalExp e2
                   return n2
  EAssgn v n -> do n' <- evalExp n
                   update v n'
                   return n'
  BTrue -> return True
  BFalse -> return False
  Lt a b -> fun a b (<)
  Gt a b -> fun a b (>)
  And a b -> fun a b (&&)
  Or a b -> fun a b (||)
  Not a -> do a' <- evalExp a
              return $ not a'
  Eq a b -> fun a b (==)
  NEq a b -> fun a b (/=)
  where
    fun :: MonadState m => Exp a -> Exp a -> (a -> a -> b) -> m b
    fun = \x y op -> do a' <- evalExp x
                        b' <- evalExp y
                        return $ op a' b'


