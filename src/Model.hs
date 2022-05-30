module Model
    (   someFunc,
        ProgramStatus(..),
        ProgramState(..),
        TuringMachine(..),
        Tape(..),
        TapeSymbol(..),
        CommandTape(..),
        transitionState,
        initializeTape,
        initializeProgram,
        emptyProgram,
        infiniteListOf,
        moveHeadRight,
        moveHeadLeft,
        readTape,
        writeTape,
        liftCommandState,
        joz,
        dot,
        jnz,
        end,
        comma      
    ) where

import Data.Char
import Data.Array
import System.Environment
import System.IO

someFunc :: IO()
someFunc = putStrLn $ show $ take 3 $ infiniteListOf 0

maxVal :: Int
maxVal = 255

data Tape a = Tape [a] a [a]

instance Show a => Show (Tape a) where
   show (Tape l p r) = "(" ++ (show $ take 5 l) ++ show p ++ (show $ take 5 r) ++ ")" 

infiniteListOf :: a -> [a]
infiniteListOf x = iterate id x

initializeTape :: a -> Tape a
initializeTape x = Tape (infiniteListOf x) x (infiniteListOf x)

-- tape view:
-- | ls | l | H:p | r | rs |
-- =>
-- | ls | l | p | H:r | rs |
-- stack view:
-- | l  |  p  | r  |
-- | ls |     | rs |
-- =>               
-- | p  |  r  | rs |
-- | l  |     
-- | ls |
moveHeadRight :: Tape a -> Tape a
moveHeadRight (Tape ls p (r:rs)) = (Tape (p:ls) r rs)
moveHeadRight _ = undefined

-- tape view:
-- | ls | l | H:p | r | rs |
-- =>
-- | ls | H:l | p | r | rs |
-- stack view:
-- | l  |  p  | r  |
-- | ls |     | rs |
-- =>               
-- | ls  |  l  | p |
--            | r  |     
--            | rs |
moveHeadLeft  :: Tape a -> Tape a
moveHeadLeft (Tape (l:ls) p rs) = (Tape ls l (p:rs))
moveHeadLeft _ = undefined

readTape :: Tape a -> a
readTape (Tape _ p _) = p

writeTape :: Tape a -> a -> Tape a
writeTape (Tape l _ r) p = (Tape l p r)


data TapeSymbol a = TapeSymbol a deriving (Show, Eq)

instance Functor (TapeSymbol) where
    fmap f (TapeSymbol g) = TapeSymbol $ f g 
 
-- Q x Gamma -> Q x Gamma x {L, R}
-- (State, Sym) -> (State, Sym, L/R)
type CommandTape i = (i, Array i (ProgramState -> IO ProgramState))
type StateTapes  s = [(Tape (TapeSymbol s))]

-- instance Show i => Show (CommandTape i) where
--     show (i, a) = "(" ++ (show i) ++ ", " ++ (show $ map (\f -> "func") a) ++ ")"

data TuringMachine = TuringMachine (CommandTape Int) (StateTapes Int)
instance Show TuringMachine where
    show (TuringMachine ct sts) = "Turing Machine: (" ++ "command tape" ++ ", " ++ (show sts) ++ ")"

initializeTM :: (CommandTape Int) -> [Tape (TapeSymbol Int)] -> TuringMachine
initializeTM ct ts = TuringMachine ct ts

comma :: ProgramState -> IO ProgramState
comma  (TuringMachine (i, cta) (t:ts), state,  ob, ib:ibs) =
    return (TuringMachine (i+1, cta) ((writeTape t $ TapeSymbol $ ord ib):ts), state, ob, ibs)
    
comma  (tm, state, ob, []) = 
    do  putStr ">"
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
    do  let TapeSymbol s = readTape t
        putStr $ [chr s]
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



data ProgramStatus = Ready | Init | Running | Stopped | Error | Input deriving Show

type OutputBuffer = String
type InputBuffer  = String

type ProgramState = (TuringMachine, --Command Tape, State Tapes
                     ProgramStatus,
                     OutputBuffer,
                     InputBuffer)      

initializeProgram :: (CommandTape Int) -> ProgramState
--make infinite tapes!!
initializeProgram ct = ((initializeTM ct $ infiniteListOf $ initializeTape $ TapeSymbol 0), Ready, [], [])

emptyProgram :: ProgramState
emptyProgram = (initializeTM (0, (listArray (0,0) [(\ps -> do return ps)])) [initializeTape $ TapeSymbol 0],
                Ready,
                [], []) 


liftCommandTM :: TuringMachine -> (ProgramState -> IO ProgramState)
liftCommandTM (TuringMachine (i, cta) (ts)) = (cta!i)

liftCommandState :: ProgramState -> (ProgramState -> IO ProgramState)
liftCommandState (tm, _, _, _) = liftCommandTM tm



-- stateMachine :: ProgramState -> TuringMachine
-- stateMachine (sm, _, _, _, _) = sm

-- commandMachine :: ProgramState -> TuringMachine
-- commandMachine (_, cm, _, _, _) = cm

{-
    1. Retrieve the command from the command machine
    2. Run the command on the state machine
-}
transitionState :: ProgramState -> IO ProgramState
transitionState ps@(_, Error, _, _) = do return ps
-- need to do more error handling with the status
transitionState (tm, status, ob, ib) = (liftCommandTM tm) (tm, Running, ob, ib)

