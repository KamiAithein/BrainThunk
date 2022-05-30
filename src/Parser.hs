module Parser
    (   tokenize,
        genCommandTape      
    ) where

import Model
import Data.Char
import Data.Array

tokenize :: String -> [TapeSymbol Char] 
-- this doesn't check if valid tape symbol!!
tokenize s = fmap (\c -> TapeSymbol c) $ filter (not . isSpace) s


genCommandTape :: [TapeSymbol Char] -> CommandTape Int
genCommandTape ss = genCommandTape' 0 ss [] [] []
    where
        genCommandTape' :: Int -> [TapeSymbol Char] -> [Int] -> [Int] -> [(ProgramState -> IO ProgramState)] -> CommandTape Int
        genCommandTape' i ((TapeSymbol '['):ss) lis      [  ] acc =
            genCommandTape' (i+1) ss (i:lis) [] ((joz $ -1):acc) -- fix!

        genCommandTape' i ((TapeSymbol ']'):ss) (li:lis) [  ] acc =
            genCommandTape' (i+1) ss lis [] ((jnz li):acc) -- fix left bracket

        genCommandTape' i ((TapeSymbol ','):ss) lis ris acc =
            genCommandTape' (i+1) ss lis ris ((comma):acc)

        genCommandTape' i ((TapeSymbol '.'):ss) lis ris acc = 
            genCommandTape' (i+1) ss lis ris ((dot):acc)

        genCommandTape' i [] [] [] acc =
            let tapeL = reverse (end:acc)
            in (0, listArray (0, i+1) tapeL)