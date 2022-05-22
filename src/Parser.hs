module Parser
    (   tokenize,
        tapeify                
    ) where

import Model
import Data.Char

tokenize :: String -> [TapeSymbol Int] 
-- this doesn't check if valid tape symbol!!
tokenize s = fmap (\c -> TapeSymbol $ ord c) $ filter (not . isSpace) s

tapeify :: (TapeSymbol a) -> [TapeSymbol a] -> Tape (TapeSymbol a)
tapeify base []     = Tape (infiniteListOf base) base (      infiniteListOf base) 
tapeify base (r:rs) = Tape (infiniteListOf base) r    (rs ++ infiniteListOf base)