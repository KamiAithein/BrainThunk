module Parser
    (   tokenize,
        genCommandTape      
    ) where

import Model
import Data.Char
import Data.Array
import qualified Data.HashMap.Strict as H (HashMap, member, insert, lookup, empty, fromList, union, size)
import qualified Data.Set as S (Set, member, fromList)

tokenize :: String -> [TapeSymbol Char] 
-- this doesn't check if valid tape symbol!!
tokenize s = fmap (\c -> TapeSymbol c) $ filter (\v -> S.member v commandTokens) s

commandTokens :: S.Set Char
commandTokens = S.fromList [',',
                            '.',
                            '[',
                            ']',
                            '<',
                            '>',
                            '+',
                            '-',
                            '\'', --up
                            ';'] --down

data CommandSub = Comma
                    | Dot
                    | Inc
                    | Dec
                    | Mvr
                    | Mvl
                    | Mvu
                    | Mvd
                    | End
                    | Jnz Int
                    | Joz Int deriving (Eq, Show)
                    
simpleCommandsSubMap :: [(TapeSymbol Char, CommandSub)]
simpleCommandsSubMap = [(TapeSymbol ',',  Comma),
                        (TapeSymbol '.',  Dot  ),
                        (TapeSymbol '+',  Inc  ),
                        (TapeSymbol '-',  Dec  ),
                        (TapeSymbol '>',  Mvr  ),
                        (TapeSymbol '<',  Mvl  ),
                        (TapeSymbol '\'', Mvu  ),
                        (TapeSymbol ';',  Mvd  )]

commandsMap :: [(CommandSub, (ProgramState -> IO ProgramState))]
commandsMap = [(Comma, (comma)),
               (Dot,   (dot  )),
               (Inc,   (inc  )),
               (Dec,   (dec  )),
               (Mvr,   (mvr  )),
               (Mvl,   (mvl  )),
               (Mvu,   (mvu  )),
               (Mvd,   (mvd  )),
               (End,   (end  ))]

genCommandTape :: [TapeSymbol Char] -> CommandTape Int
genCommandTape ss = genCommandTape' 0 ss [] [] []
    where
        genCommandTape' :: Int -> [TapeSymbol Char] -> [Int] -> [Int] -> [CommandSub] -> CommandTape Int
        genCommandTape' i ((TapeSymbol '['):ss) lis      [  ] acc =
            genCommandTape' (i+1) ss (i:lis) [] ((Joz $ -1):acc) -- fix!

        genCommandTape' i ((TapeSymbol ']'):ss) (li:lis) [  ] acc =
            genCommandTape' (i+1) ss lis [] ((Jnz li):acc) -- fix left bracket

        genCommandTape' i [] [] [] acc =
            let interList = zip [0..] $ reverse (End:acc)
                interMap =  (fix (i) $ H.fromList $ interList)
                tapeL =  map mapper interMap
            in (0, listArray (0, i+1) tapeL)
            where fix (-1) ss = [let Just s = H.lookup i ss in s | i <- [0..((H.size ss) - 1)]]
                  fix i  ss = let Just sym = H.lookup i ss
                              in case sym of
                                  Jnz ji -> let ss' = H.insert ji (Joz i) ss
                                            in fix (i-1) ss'
                                  _ -> fix (i-1) ss
                  mapper sub = case (lookup sub commandsMap) of 
                                    Just cmd -> cmd
                                    Nothing -> case sub of
                                        Jnz i -> jnz i
                                        Joz i -> joz i
                                        _ -> undefined


        genCommandTape' i (s:ss) lis ris acc =
            case (lookup s simpleCommandsSubMap) of
                Just cmd -> genCommandTape' (i+1) ss lis ris ((cmd):acc)
                Nothing  -> genCommandTape' (i) ss lis ris acc