module Model
    (   ProgramStatus(..),
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
        comma,
        inc,
        dec,
        mvl,
        mvr,
        mvu,
        mvd
    ) where

import Data.Char
import Data.Array
import System.Environment
import System.IO

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
data StateTape  s = StateTape [(Tape (TapeSymbol s))] (Tape (TapeSymbol s)) [(Tape (TapeSymbol s))]

instance (Show s) => Show (StateTape s) where
    show (StateTape l p r) = "(" ++ (show $ take 5 l) ++ ", " ++ (show p) ++ ", " ++ (show $ take 5 r) ++ ")"

initializeStateTape :: s -> StateTape s
initializeStateTape s = StateTape (infiniteListOf $ initializeTape $ TapeSymbol s) (initializeTape $ TapeSymbol s) (infiniteListOf $ initializeTape $ TapeSymbol s)

writeStateTape :: StateTape s -> s -> StateTape s
writeStateTape (StateTape l p r) p' = (StateTape l (writeTape p $ TapeSymbol p') r)

readStateTape :: StateTape s -> s
readStateTape (StateTape _ p _) = let TapeSymbol s = readTape p in s

moveHeadUp :: StateTape s -> StateTape s
moveHeadUp (StateTape (l:ls) p rs) = (StateTape ls l (p:rs))

moveHeadDown :: StateTape s -> StateTape s
moveHeadDown (StateTape ls p (r:rs)) = (StateTape (p:ls) r rs)

-- instance Show i => Show (CommandTape i) where
--     show (i, a) = "(" ++ (show i) ++ ", " ++ (show $ map (\f -> "func") a) ++ ")"

data TuringMachine = TuringMachine (CommandTape Int) (StateTape Int)
instance Show TuringMachine where
    show (TuringMachine ct sts) = "Turing Machine: (" ++ "command tape" ++ ", " ++ (show sts) ++ ")"

initializeTM :: (CommandTape Int) -> (StateTape Int) -> TuringMachine
initializeTM ct ts = TuringMachine ct ts

comma :: ProgramState -> IO ProgramState
comma  (TuringMachine (i, cta) (sts), state,   ib:ibs) =
    return (TuringMachine (i+1, cta) (writeStateTape sts $ ord ib), state,  ibs)
    
comma  (tm, state,  []) = 
    do  putStr ">"
        hFlush stdout
        eof <- isEOF
        if eof
            then comma (tm, Running,  "\0")
            else do 
                input <- getLine
                comma (tm, Running,  input ++ "\n")
joz :: Int -> ProgramState -> IO ProgramState
joz i' (TuringMachine (i, cta) sts@(StateTape sl (Tape pl (TapeSymbol 0) pr) sr), state,  ib) = 
    return (TuringMachine (i', cta)  (sts), state,  ib)

joz _  (TuringMachine (i, cta) (sts), state,  ib) = 
    return (TuringMachine (i+1, cta) (sts), state,    ib)

dot :: ProgramState -> IO ProgramState
dot    (TuringMachine (i, cta) (sts), state,   ib) =
    do  let s = readStateTape sts
        putStr $ [chr s]
        hFlush stdout
        return (TuringMachine (i+1, cta) (sts), state,  ib)

jnz :: Int -> ProgramState -> IO ProgramState
jnz _  (TuringMachine (i, cta) sts@(StateTape sl (Tape pl (TapeSymbol 0) pr) sr), state,  ib) = 
    return (TuringMachine (i+1, cta) (sts), state,    ib)    

jnz i' (TuringMachine (i, cta) (sts), state,  ib) = 
    return (TuringMachine (i',  cta) (sts), state,  ib)

end :: ProgramState -> IO ProgramState
end    (TuringMachine (i, cta) (ts), state,  ib) =
    return (TuringMachine (i,   cta) (ts), Stopped,  ib)

inc :: ProgramState -> IO ProgramState
inc    (TuringMachine (i, cta) (StateTape sl (Tape pl pp pr) sr), state,  ib) = 
    return (TuringMachine (i+1,   cta) (StateTape sl (Tape pl (fmap (+1) pp) pr) sr), state,  ib)

dec :: ProgramState -> IO ProgramState
dec    (TuringMachine (i, cta) (StateTape sl (Tape pl pp pr) sr), state,  ib) = 
    return (TuringMachine (i+1,   cta) (StateTape sl (Tape pl (fmap (subtract 1) pp) pr) sr), state,  ib)

mvl :: ProgramState -> IO ProgramState
mvl (TuringMachine (i, cta) (StateTape sl sp sr), state,  ib) =
    return (TuringMachine (i+1,   cta) (StateTape sl (moveHeadLeft sp) sr), state,  ib)

mvr :: ProgramState -> IO ProgramState
mvr (TuringMachine (i, cta) (StateTape sl sp sr), state,  ib) =
    return (TuringMachine (i+1,   cta) (StateTape sl (moveHeadRight sp) sr), state,  ib)

mvu :: ProgramState -> IO ProgramState
mvu (TuringMachine (i, cta) sts, state,  ib) =
    return (TuringMachine (i+1, cta) $ moveHeadUp sts, state, ib)

mvd :: ProgramState -> IO ProgramState
mvd (TuringMachine (i, cta) sts, state,  ib) =
    return (TuringMachine (i+1, cta) $ moveHeadDown sts, state, ib)


data ProgramStatus = Ready | Init | Running | Stopped | Error | Input deriving Show

type InputBuffer  = String

type ProgramState = (TuringMachine, --Command Tape, State Tapes
                     ProgramStatus,
                     InputBuffer)      

initializeProgram :: (CommandTape Int) -> ProgramState
--make infinite tapes!!
initializeProgram ct = ((initializeTM ct $ initializeStateTape 0), Ready, [])

emptyProgram :: ProgramState
emptyProgram = (initializeTM (0, (listArray (0,0) [(\ps -> do return ps)])) $ initializeStateTape 0,
                Ready,
                []) 


liftCommandTM :: TuringMachine -> (ProgramState -> IO ProgramState)
liftCommandTM (TuringMachine (i, cta) (ts)) = (cta!i)

liftCommandState :: ProgramState -> (ProgramState -> IO ProgramState)
liftCommandState (tm, _, _) = liftCommandTM tm


transitionState :: ProgramState -> IO ProgramState
transitionState ps@(_, Error, _) = do return ps
-- need to do more error handling with the status
transitionState (tm, status,  ib) = (liftCommandTM tm) (tm, Running,  ib)

