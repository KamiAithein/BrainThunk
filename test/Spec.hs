import Model
import Data.Array

--
import Parser
import Data.Char
import System.Environment
import System.IO

-- ,[.,]
initEchoCT :: CommandTape Int
initEchoCT = (0, listArray (0, 5) [(comma),
                                   (joz 4),
                                   (dot),
                                   (comma),
                                   (jnz 1),
                                   (end)])
           where comma :: ProgramState -> IO ProgramState
                 comma  (TuringMachine (i, cta) (t:ts), state,  ob, ib:ibs) =
                        return (TuringMachine (i+1, cta) ((writeTape t $ TapeSymbol $ ord ib):ts), state, ob, ibs)
                     
                 comma  (tm, state, ob, []) = 
                     do putStr ">"
                        hFlush stdout
                        eof <- isEOF
                        if eof
                            then comma (tm, Running, ob, "\0")
                            else do 
                                input <- getLine
                                comma (tm, Running, ob, input ++ "\n")
                 joz :: Int -> ProgramState -> IO ProgramState
                 joz i' (TuringMachine (i, cta) (t@(Tape l (TapeSymbol 0) r):ts), Running, ob, ib) = 
                        return (TuringMachine (i', cta)  (ts), Running, ob, ib)

                 joz _  (TuringMachine (i, cta) (ts), state, ob, ib) = 
                        return (TuringMachine (i+1, cta) (ts), state,   ob, ib)
                 
                 dot :: ProgramState -> IO ProgramState
                 dot    (TuringMachine (i, cta) (t:ts), state,  ob, ib) =
                     do let TapeSymbol s = readTape t
                        putStr $ show $ chr s
                        hFlush stdout
                        return (TuringMachine (i+1, cta) (ts), state, ob, ib)
                 
                 jnz :: Int -> ProgramState -> IO ProgramState
                 jnz _  (TuringMachine (i, cta) (t@(Tape l (TapeSymbol 0) r):ts), state, ob, ib) = 
                        return (TuringMachine (i+1, cta) (ts), state,   ob, ib)    

                 jnz i' (TuringMachine (i, cta) (ts), Running, ob, ib) = 
                        return (TuringMachine (i',  cta) (ts), Running, ob, ib)
                 
                 end :: ProgramState -> IO ProgramState
                 end    (TuringMachine (i, cta) (ts), Running, ob, ib) =
                        return (TuringMachine (i,   cta) (ts), Stopped, ob, ib)

initEchoTM :: TuringMachine
initEchoTM = TuringMachine (initEchoCT) (infiniteListOf $ initializeTape (TapeSymbol 0))

initEcho :: ProgramState
initEcho = (initEchoTM,
            Ready,
            [],
            [])

simulateProgram :: IO ProgramState -> IO ProgramState
simulateProgram program = do program <- program
                             case program of
                                 state@(_, Stopped, _, _) -> return state
                                 state -> simulateProgram $ (liftCommandState state) state

main :: IO ()
main = do
    let ps = initEcho
    simulateProgram $ return ps
    return $ ()