module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )
import           Prelude                 hiding ( fst
                                                , snd
                                                )
-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 

newtype StateErrorTrace a = StateErrorTrace { runStateErrorTrace :: Env -> Either Error (Pair a (Pair Env Trace)) }

instance Monad  StateErrorTrace where
  return x = StateErrorTrace (\s -> Right (x :!: (s :!: [])))
  m >>= f = StateErrorTrace (\s -> case runStateErrorTrace m s of
                                      Right (a :!: (e :!: t)) -> case runStateErrorTrace (f a) e of
                                                                    Right (a' :!: (e' :!: t')) -> Right (a' :!: (e' :!: t ++ t'))
                                                                    Left err -> Left err
                                      Left err -> Left err)   

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor  StateErrorTrace where
  fmap = liftM

instance Applicative  StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  trace t = StateErrorTrace (\e -> Right (() :!: (e :!: t)))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = StateErrorTrace (\s -> Left e)

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\e -> case lookfor' v e of
                Just n -> Right (n :!: (e :!: [])) 
                _      -> Left UndefVar)
    where lookfor' v s = M.lookup v s
  update v i = StateErrorTrace (\s -> Right (() :!: (update' v i s :!: mempty))) where update' = M.insert

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either Error (Pair Env Trace)
eval c = case runStateErrorTrace (stepCommStar c) initEnv of
            Right t -> Right $ snd t 
            Left err -> Left err

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadTrace m, MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadTrace m, MonadState m, MonadError m) => Comm -> m Comm
stepComm c = case c of
                Skip -> return Skip
                Let v i -> do n <- evalExp i
                              update v n
                              trace $ "Let " ++ v ++ " " ++ (show n) ++ "; "
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
evalExp :: (MonadTrace m, MonadError m, MonadState m) => Exp a -> m a
evalExp e = case e of 
  Const n -> return n
  Var v -> do val <- lookfor v
              return val
  UMinus n -> do n' <- evalExp n
                 return $ -n'
  Plus a b -> fun a b (+)
  Minus a b -> fun a b (-)
  Times a b -> fun a b (*)
  Div a b -> do a' <- evalExp a
                b' <- evalExp b
                if b' == 0 then throw DivByZero
                           else return $ div a' b'
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
    fun :: (MonadTrace m, MonadState m, MonadError m) => Exp a -> Exp a -> (a -> a -> b) -> m b
    fun = \x y op -> do a' <- evalExp x
                        b' <- evalExp y
                        return $ op a' b'

