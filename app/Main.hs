module Main where

import Model
import Parser
import Data.Char
import System.Environment
import System.IO

simulateProgram :: IO ProgramState -> IO ProgramState
simulateProgram program = do program' <- program
                             let program'' = transitionState program'
                            --  putStrLn $ show $ program''
                             case program'' of
                                 (tm, Running, ob, ib) -> do putStr ob
                                                             hFlush stdout
                                                             simulateProgram $ return (tm, Running, [], ib)
                                 (tm, Input,   ob, ib) -> do putStr ">"
                                                             hFlush stdout
                                                             eof <- isEOF
                                                             if eof
                                                                 then simulateProgram $ return (tm, Running, ob, ib ++ "\0")
                                                                 else do 
                                                                     input <- getLine
                                                                     simulateProgram $ return (tm, Running, ob, ib ++ input ++ "\n")
                                 _ -> return program'' 

run :: String -> IO ProgramState
run code = 
    do let tokens        = tokenize code
       let commandTape   = tapeify (TapeSymbol 0) tokens
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
    

