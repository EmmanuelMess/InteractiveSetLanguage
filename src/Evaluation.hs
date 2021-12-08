module Evaluation where

import           AST
import           Data.Range
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Control.Monad                  ( liftM
                                                , ap
                                                )

data Error = UndefinedVariable Variable | WeirdEmptySet Variable

class Monad m => MonadState m where
  -- Busca el valor de una variable
  lookfor :: Variable -> m Set
  -- Cambia el valor de una variable
  update :: Variable -> Set -> m ()

-- Entornos
type Env = M.Map Variable Set

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> (Either [Error] a, Env) }

instance Monad State where
  return x = State (\s -> (Right x, s))
  m >>= f  = State (\s ->
                    let (ev, s') = runState m s
                    in case ev of
                      Left e  -> (Left e, s')
                      Right v -> runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s ->
                let x = M.lookup v s
                in case x of
                  Nothing  -> (Left [UndefinedVariable v], s)
                  Just set -> (Right set, s))
  update v i = State (\s -> (Right (), M.insert v i s))

eval :: Env -> Comm -> (Env, Either [Error] String)
eval env p = let (ve, env') = runState (stepCommStar p) env
             in  (env', ve)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m String
stepCommStar Skip              = return ""
stepCommStar (PrintVariable x) = do 
  a <- evalExp x
  return (show a)
stepCommStar (PrintBoolean x)  = return (show x)
stepCommStar c                 = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm Skip          = return Skip
stepComm (In y c1)     = do
 x <- evalExp c1
 return (PrintBoolean (inRanges x y))
stepComm (Let v e)     = do 
  x <- evalExp e
  update v x
  return Skip

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
-- Set
evalExp (Var c1)               = lookfor c1
evalExp Empty                  = return []
evalExp Universe               = return [inf]
evalExp (Range x y)            = return [x +=+ y]
evalExp (Elements x c1)        = do
  y <- evalExp c1
  return (SingletonRange x:y)
evalExp (SpacedElements x y z) = return (map SingletonRange [x, y..z])
evalExp (Union c1 c2)          = do
  x <- evalExp c1
  y <- evalExp c2
  return (union x y)
evalExp (Difference c1 c2)     = do 
  x <- evalExp c1
  y <- evalExp c2
  return (difference x y)
evalExp (Intersection c1 c2)   = do
  x <- evalExp c1
  y <- evalExp c2
  return (intersection x y)
evalExp (Complement c1)        = do 
  x <- evalExp c1
  return (invert x)
