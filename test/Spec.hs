import Model


main :: IO ()
main = do
    let tape1 = initializeTape 0
    let tape2 = writeTape tape1 1
    let tape3 = moveHeadLeft tape2
    let tape4 = writeTape tape3 2
    putStrLn $ show $ readTape tape4
    let tape5 = moveHeadRight tape4
    putStrLn $ show $ readTape tape5
    let tape6 = (moveHeadLeft . moveHeadLeft) tape5
    putStrLn $ show $ readTape tape6
