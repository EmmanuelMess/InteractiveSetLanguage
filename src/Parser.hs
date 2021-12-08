module Parser where

import           AST

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )

---------------------------------------------------------

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentLine     = "--"
    , opLetter        = char '='
    , reservedNames   = ["universe", "empty", "exit"]
    , reservedOpNames = [ "+", "-", "^", "in", "[", "]", "{", "}", ",", "..."]
    }
  )

setExpressions :: Parser (Exp Set)
setExpressions = simpleSetExpressions `chainl1` binarySetExpressions

binarySetExpressions :: Parser (Exp Set -> Exp Set -> Exp Set)
binarySetExpressions =
  do
    reservedOp lis "+"
    return Union
  <|> do
    reservedOp lis "-"
    return Difference
  <|> do
    reservedOp lis "^"
    return Intersection

simpleSetExpressions :: Parser (Exp Set)
simpleSetExpressions =
  do
    reservedOp lis "universe"
    return Universe
  <|> do
    reservedOp lis "empty"
    return Empty
  <|> do
    reservedOp lis "["
    start <- float lis
    reservedOp lis ","
    end <- float lis
    reservedOp lis "]"
    return (Range start end)
  <|> do
    reservedOp lis "{"
    values <- getInsideSet
    reservedOp lis "}"
    return values
  <|> do
    v <- identifier lis
    return (Var v)
  <|> do
    reservedOp lis "-"
    x <- simpleSetExpressions
    return (Complement x)

getInsideSet :: Parser (Exp Set)
getInsideSet =
  do
    x <- float lis
    reservedOp lis ","
    y <- float lis
    reservedOp lis ","
    reservedOp lis "..."
    reservedOp lis ","
    z <- float lis
    return (SpacedElements x y z)
  <|> do
    values <- getValues
    return values
  <|> do
    return Empty

getValues :: Parser (Exp Set)
getValues =
  do
    value <- float lis
    do 
      reservedOp lis ","
      nextValues <- getValues
      return (Elements value nextValues)
     <|> do
       return (Elements value Empty)
    
comm :: Parser Comm
comm =
    do
      reservedOp lis "exit"
      return Exit
    <|> do
      reservedOp lis "print"
      valor <- setExpressions
      return (PrintVariable valor)
    <|> do
      nombre <- identifier lis
      reservedOp lis "="
      valor <- setExpressions
      return (Let nombre valor)
    <|> do
      x <- float lis
      reservedOp lis "in"
      s <- setExpressions
      return (In x s)


