module Model
    (   someFunc,
        ProgramStatus(..),
        ProgramState(..),
        TuringMachine(..),
        Tape(..),
        TapeSymbol(..),
        transitionState,
        stateMachine,
        commandMachine,
        initializeTape,
        initializeProgram,
        emptyProgram,
        infiniteListOf,
        moveHeadRight,
        moveHeadLeft,
        readTape,
        writeTape                
    ) where

import Data.Char

someFunc :: IO()
someFunc = putStrLn $ show $ take 3 $ infiniteListOf 0

maxVal :: Int
maxVal = 255

data Tape a = Tape [a] a [a]

instance Show (Tape a) where
   show (Tape l p r) = "(" ++ (show take 5 l) ++ show p ++ (show take 5 r) ++ ")" 

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


data TapeSymbol a = TapeSymbol a deriving Show

instance Functor (TapeSymbol) where
    fmap f (TapeSymbol g) = TapeSymbol $ f g 
 
data TuringMachine = TuringMachine [(Tape (TapeSymbol Int))] 
instance Show TuringMachine where
    show (TuringMachine tapes) = "Turing Machine: (" ++ show tapes ++ ")"

initializeTM :: [Tape (TapeSymbol Int)] -> TuringMachine
initializeTM ts = TuringMachine ts

-- BF brain
-- transitionTM :: TuringMachine -> TuringMachine
-- transitionTM (TuringMachine (Tape l p r)) = 

{-
    Specifications:
        Take input from user on shell
            User from the input should somehow be put onto the command state
            Easiest implementation right now is to replace the command machine on every input and run to end



        Read BF program file
-}
data ProgramStatus = Ready | Init | Running | Stopped | Error | Input deriving Show

type OutputBuffer = String
type InputBuffer  = String

type ProgramState = (TuringMachine, --Command Tape, State Tapes
                     ProgramStatus,
                     OutputBuffer,
                     InputBuffer)      

initializeProgram :: Tape (TapeSymbol Int) -> ProgramState
--make infinite tapes!!
initializeProgram commandTape = (initializeTM $ [commandTape, initializeTape $ TapeSymbol 0], Ready, [], [])

emptyProgram :: ProgramState
emptyProgram = (initializeTM [initializeTape $ TapeSymbol 0,
                              initializeTape $ TapeSymbol 0],
                Ready,
                [], []) 
{- This does not take infinite tapes into account: They may exist with this implementation but no way to access
        Ideally we would implement a map solution for "indexing" into some tapes -}
-- move right
transitionMVR :: ProgramState -> ProgramState
transitionMVR (TuringMachine (ct:st:sts), status, ob, ib) = (TuringMachine (ct:(transitionMVR' st):sts) , status, ob, ib)
    where transitionMVR' t = moveHeadRight t
-- move left
transitionMVL :: ProgramState -> ProgramState
transitionMVL (TuringMachine (ct:st:sts), status, ob, ib) = (TuringMachine (ct:(transitionMVL' st):sts), status, ob, ib)
    where transitionMVL' t = moveHeadLeft t
-- increment
transitionINC :: ProgramState -> ProgramState
transitionINC (TuringMachine (ct:st:sts), status, ob, ib) = (TuringMachine (ct:(transitionINC' st):sts), status, ob, ib)
    where transitionINC' t = ((writeTape t) . fmap (addOneClamped) . readTape) t
          addOneClamped v | v == maxVal  = 0
                          | otherwise = v + 1
-- decrement
transitionDEC :: ProgramState -> ProgramState
transitionDEC (TuringMachine (ct:st:sts), status, ob, ib) = (TuringMachine (ct:(transitionDEC' st):sts), status, ob, ib)
    where transitionDEC' t = (writeTape t) . fmap (subOneClamped) . readTape) t
          subOneClamped v | v == 0    = maxVal
                          | otherwise = v - 1
-- record
transitionREC :: ProgramState -> ProgramState
transitionREC (TuringMachine (ct:st:sts), status, ob, ib) = (TuringMachine (ct:st:sts), status, ob ++ (symbolize $ readTape st), ib)
    where symbolize (TapeSymbol s) = [chr s]
-- replace
transitionREP :: ProgramState -> ProgramState
transitionREP (tm, status, ob, [])   = (tm, Input, ob, [])
transitionREP (TuringMachine (ct:st:sts), status, ob, i:is) = (TuringMachine (ct:(writeTape st $ TapeSymbol $ ord i):sts), Running, ob, is)

-- jump on zero
transitionJOZ :: ProgramState -> ProgramState
transitionJOZ state@(TuringMachine (ct:(Tape l (TapeSymbol 0) r):sts), _, _, _) = transitionJOZ' state 0
    where transitionJOZ' (tm, status, ob, ib) count | count < 0 = (tm, Error, ob, ib)
          transitionJOZ'       (TuringMachine (ct:st@(Tape _ (TapeSymbol 91{-[-}) _):sts), status, ob, ib) count = 
                transitionJOZ' (TuringMachine ((moveHeadRight ct):st:sts), status, ob, ib) (count+1)

          transitionJOZ' state@(TuringMachine (ct:(Tape _ (TapeSymbol 93{-]-}) _):sts), _, _, _) 1 = state

          transitionJOZ'       (TuringMachine (ct:st@(Tape _ (TapeSymbol 93{-]-}) _):sts), status, ob, ib) count  = 
              transitionJOZ' (TuringMachine ((moveHeadRight ct):st:sts), status, ob, ib) (count-1)

          transitionJOZ'       (TuringMachine (ct:st:sts), status, ob, ib)                                 count = 
              transitionJOZ' (TuringMachine ((moveHeadRight ct):st:sts), status, ob, ib) count
transitionJOZ state = state


--Still have to update JNZ and below
-- jump not zero
transitionJNZ :: ProgramState -> ProgramState
transitionJNZ state@(TuringMachine (Tape l (TapeSymbol 0) r), _, _, _, _) = state
transitionJNZ state = transitionJNZ' state 0 
    where transitionJNZ' (sm, cm, status, ob, ib) count | count < 0 = (sm, cm, Error, ob, ib) 

          transitionJNZ'       (sm, TuringMachine t@(Tape _ (TapeSymbol 93{-]-}) _), status, ob, ib) count = 
                transitionJNZ' (sm, (TuringMachine $ moveHeadLeft t), status, ob, ib) (count+1)

          transitionJNZ' state@(_, (TuringMachine   (Tape _ (TapeSymbol 91{-[-}) _)), _, _, _) 1 = state

          transitionJNZ'       (sm, (TuringMachine t@(Tape _ (TapeSymbol 91{-[-}) _)), status, ob, ib) count  = 
              transitionJNZ' (sm, (TuringMachine $ moveHeadLeft t), status, ob, ib) (count-1)

          transitionJNZ'       (sm, (TuringMachine t), status, ob, ib)                                 count = 
              transitionJNZ' (sm, (TuringMachine $ moveHeadLeft t), status, ob, ib) count

transitionSTP :: ProgramState -> ProgramState
transitionSTP (tm, status, ob, ib) = (tm, Stopped, ob, ib)

commandsMap :: [(Char, ProgramState -> ProgramState)]
commandsMap = [('>',    transitionMVR),
               ('<',    transitionMVL),
               ('+',    transitionINC),
               ('-',    transitionDEC),
               ('.',    transitionREC),
               (',',    transitionREP),
               ('[',    transitionJOZ),
               (']',    transitionJNZ),
               (chr 0,  transitionSTP)]


liftCommandChar :: Char -> (ProgramState -> ProgramState)
liftCommandChar tok = 
    case (lookup tok commandsMap) of
        Just transition -> transition
        Nothing         -> id

liftCommandTM :: TuringMachine -> (ProgramState -> ProgramState)
liftCommandTM (TuringMachine t:ts) = liftCommandChar (currentTok $ readTape t)
    where currentTok (TapeSymbol sym) = chr sym



-- stateMachine :: ProgramState -> TuringMachine
-- stateMachine (sm, _, _, _, _) = sm

-- commandMachine :: ProgramState -> TuringMachine
-- commandMachine (_, cm, _, _, _) = cm

{-
    1. Retrieve the command from the command machine
    2. Run the command on the state machine
-}
transitionState :: ProgramState -> ProgramState
transitionState ps@(_, Error, _, _) = ps
-- need to do more error handling with the status
transitionState (tm, status, ob, ib) = handleTransition $ (liftCommandTM tm) (tm, Running, ob, ib) 
    where handleTransition state@(_, Input, _, _)   = state
          handleTransition state@(_, Stopped, _, _) = state
          handleTransition (TuringMachine (ct:ts), _, ob, ib) = (TuringMachine ((moveHeadRight ct):ts), Running, ob, ib)
          handleTransition _ = undefined

