module Main where

import Model
import Parser
import Data.Char
import System.Environment
import System.IO
import Debug.Trace

run :: String -> IO ProgramState
run code = 
    do let tokens        = tokenize code
       let commandTape   = genCommandTape tokens
       let program       = initializeProgram $ commandTape
       simulateProgram $ return program
                      
repl :: IO ()
repl = 
    do input <- getLine
       run input      
       repl

exec :: String -> IO ()
exec filename = 
    do code <- readFile filename
       run code
       putStrLn ""
       putStrLn "Complete!"

main :: IO ()
main = 
    do args <- getArgs
       case args of
           ["repl"]        -> repl
           [filename]      -> exec filename 
           _ -> undefined
    

simulateProgram :: IO ProgramState -> IO ProgramState
simulateProgram program = do program <- program
                             case program of
                                 state@(_, Stopped, _, _) -> return state
                                 state -> simulateProgram $ (liftCommandState state) state
