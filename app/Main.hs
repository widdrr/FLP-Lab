
module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Sugar
import Eval
import Printing
import REPLCommand
import Program

main :: IO ()
main = execute empty
  
execute :: Environment -> IO ()
execute env
  = do
    putStr "miniHaskell> "
    hFlush stdout
    s <- getLine
    case parseFirst replCommand s of
          Nothing -> putStrLn "Cannot parse command" >> execute env
          Just Quit -> return ()
          Just (Load path) ->
            case parseFirst string path of
              Nothing -> putStrLn "Invalid file path" >> execute env 
              Just p -> parseFromFile program p >>= \list -> programEnv list  >>= \ev -> execute ev
          Just (Eval es) ->
            case parseFirst exprParser es of
              Nothing -> putStrLn "Error: cannot parse expression" >> execute env
              Just e ->
                let simpleE = desugarExp e
                    simpleE' = normalizeEnv env simpleE
                    e' = sugarExp simpleE'
                 in putStrLn (showExp e') >> execute env
