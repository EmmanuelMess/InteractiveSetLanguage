module Main where

import           System.Console.GetOpt
import qualified System.Environment            as Env
import           System.Exit
import           Control.Monad                  ( when )

import           AST
import           Parser
import           Evaluation

import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           Text.Parsec.Combinator

---------------------------------------------------------

-----------------------

printError (UndefinedVariable name) = "Error: " ++ name ++ " not declared"
printError (WeirdEmptySet     name) = "Warning: " ++ name ++ " is empty but {} was not used to declare it"

main :: IO ()
main = runInputT defaultSettings (loop initEnv)
        where
          loop :: Env -> InputT IO ()
          loop env = do
            minput <- getInputLine "ISL> "
            case minput of
              Nothing -> return ()
              Just "" -> loop env
              Just input -> do
                case parse (totParser comm) "" input of
                  Left error -> do
                    outputStrLn (show error)
                    loop env
                  Right Exit -> return ()
                  Right x    -> do
                    (case eval env x of
                     (env', Left errors) -> do
                       outputStrLn (concatMap printError errors)
                       loop env'
                     (env', Right "") -> loop env'
                     (env', Right s)  -> do
                       outputStrLn s
                       loop env')