module AST where

import           Data.Range

-- Identificadores de Variable
type Variable = String
type Set = [Range Double]

-- Expresiones, aritmeticas y booleanas
data Exp a where
  -- Set
  Var ::Variable -> Exp Set
  Empty ::Exp Set
  Universe ::Exp Set
  Range ::Double -> Double -> Exp Set
  Elements ::Double -> Exp Set -> Exp Set
  SpacedElements ::Double -> Double -> Double -> Exp Set
  Union ::Exp Set -> Exp Set -> Exp Set
  Difference ::Exp Set -> Exp Set -> Exp Set
  Intersection ::Exp Set -> Exp Set -> Exp Set
  Complement ::Exp Set -> Exp Set

deriving instance Show (Exp a)
deriving instance Eq (Exp a)

-- Comandos (sentencias)
data Comm
  = Skip
  | Exit
  | In Double (Exp Set)
  | PrintVariable (Exp Set)
  | PrintBoolean Bool
  | Let Variable (Exp Set)
  deriving (Show, Eq)