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

data Tape a = Tape [a] a [a] deriving Show

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
 
data TuringMachine = TuringMachine (Tape (TapeSymbol Int)) 
instance Show TuringMachine where
    show (TuringMachine (Tape l p r)) = "{l: " ++ (show $ take 3 l) ++ ", p: " ++ show p ++ ", r: " ++ (show $ take 3 r) ++ "}"

initializeTM :: Tape (TapeSymbol Int) -> TuringMachine
initializeTM t = TuringMachine t

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

type ProgramState = (TuringMachine,      -- State   Machine
                     TuringMachine,      -- Command Machine
                     ProgramStatus,
                     OutputBuffer,
                     InputBuffer)      

initializeProgram :: Tape (TapeSymbol Int) -> ProgramState
initializeProgram commandTape = (initializeTM $ initializeTape $ TapeSymbol 0, initializeTM $ commandTape, Ready, [], [])

emptyProgram :: ProgramState
emptyProgram = (initializeTM $ initializeTape $ TapeSymbol 0,
                initializeTM $ initializeTape $ TapeSymbol 0,
                Ready,
                [], []) 

-- move right
transitionMVR :: ProgramState -> ProgramState
transitionMVR (sm, cm, status, ob, ib) = (transitionMVR' sm, cm, status, ob, ib)
    where transitionMVR' (TuringMachine t) = (TuringMachine $ moveHeadRight t)
-- move left
transitionMVL :: ProgramState -> ProgramState
transitionMVL (sm, cm, status, ob, ib) = (transitionMVL' sm, cm, status, ob, ib)
    where transitionMVL' (TuringMachine t) = (TuringMachine $ moveHeadLeft t)
-- increment
transitionINC :: ProgramState -> ProgramState
transitionINC (sm, cm, status, ob, ib) = (transitionINC' sm, cm, status, ob, ib)
    where transitionINC' (TuringMachine t) = (TuringMachine $ ((writeTape t) . fmap (addOneClamped) . readTape) t)
          addOneClamped v | v == maxVal  = 0
                          | otherwise = v + 1
-- decrement
transitionDEC :: ProgramState -> ProgramState
transitionDEC (sm, cm, status, ob, ib) = (transitionDEC' sm, cm, status, ob, ib)
    where transitionDEC' (TuringMachine t) = (TuringMachine $ ((writeTape t) . fmap (subOneClamped) . readTape) t)
          subOneClamped v | v == 0    = maxVal
                          | otherwise = v - 1
-- record
transitionREC :: ProgramState -> ProgramState
transitionREC (sm@(TuringMachine t), cm, status, ob, ib) = (sm, cm, status, ob ++ (symbolize $ readTape t), ib)
    where symbolize (TapeSymbol s) = [chr s]
-- replace
transitionREP :: ProgramState -> ProgramState
transitionREP (sm, cm, status, ob, [])   = (sm, cm, Input, ob, [])
transitionREP (sm@(TuringMachine t, cm, status, ob, i:is)) = (TuringMachine $ writeTape t $ TapeSymbol $ ord i, cm, Running, ob, is)

-- jump on zero
transitionJOZ :: ProgramState -> ProgramState
transitionJOZ state@(TuringMachine (Tape l (TapeSymbol 0) r), _, _, _, _) = transitionJOZ' state 0
    where transitionJOZ' (sm, cm, status, ob, ib) count | count < 0 = (sm, cm, Error, ob, ib)
          transitionJOZ'       (sm, TuringMachine t@(Tape _ (TapeSymbol 91{-[-}) _), status, ob, ib) count = 
                transitionJOZ' (sm, (TuringMachine $ moveHeadRight t), status, ob, ib) (count+1)

          transitionJOZ' state@(_, (TuringMachine   (Tape _ (TapeSymbol 93{-]-}) _)), _, _, _) 1 = state

          transitionJOZ'       (sm, (TuringMachine t@(Tape _ (TapeSymbol 93{-]-}) _)), status, ob, ib) count  = 
              transitionJOZ' (sm, (TuringMachine $ moveHeadRight t), status, ob, ib) (count-1)

          transitionJOZ'       (sm, (TuringMachine t), status, ob, ib)                                 count = 
              transitionJOZ' (sm, (TuringMachine $ moveHeadRight t), status, ob, ib) count
transitionJOZ state = state


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
transitionSTP (sm, cm, status, ob, ib) = (sm, cm, Stopped, ob, ib)

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
liftCommandTM (TuringMachine t) = liftCommandChar (currentTok $ readTape t)
    where currentTok (TapeSymbol sym) = chr sym



stateMachine :: ProgramState -> TuringMachine
stateMachine (sm, _, _, _, _) = sm

commandMachine :: ProgramState -> TuringMachine
commandMachine (_, cm, _, _, _) = cm

{-
    1. Retrieve the command from the command machine
    2. Run the command on the state machine
-}
transitionState :: ProgramState -> ProgramState
transitionState ps@(_, _, Error, _, _) = ps
-- need to do more error handling with the status
transitionState (sm, cm@(TuringMachine t), status, ob, ib) = handleTransition $ (liftCommandTM cm) (sm, cm, Running, ob, ib) 
    where handleTransition state@(_, _, Input, _, _)   = state
          handleTransition state@(_, _, Stopped, _, _) = state
          handleTransition (sm, TuringMachine t, _, ob, ib) = (sm, TuringMachine $ moveHeadRight t, Running, ob, ib)
          handleTransition _ = undefined

