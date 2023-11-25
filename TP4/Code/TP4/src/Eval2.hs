module Eval2
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

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\s -> Right (x :!: s))
  m >>= f = StateError (\s -> case runStateError m s of
                                Right (a :!: s') -> runStateError (f a) s'
                                Left err -> Left err)

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw e = StateError (\s -> Left e)


-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case lookfor' v s of
                Just n -> Right (n :!: s) 
                _      -> Left UndefVar)
    where lookfor' v s = M.lookup v s
  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval c =  case runStateError (stepCommStar c) initEnv of
            Right t -> Right $ snd t 
            Left err -> Left err

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
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
    fun :: (MonadState m, MonadError m) => Exp a -> Exp a -> (a -> a -> b) -> m b
    fun = \x y op -> do a' <- evalExp x
                        b' <- evalExp y
                        return $ op a' b'
