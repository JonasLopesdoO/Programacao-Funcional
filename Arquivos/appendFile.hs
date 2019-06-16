import System.IO

main = do
    escreveItem <- getLine
    appendFile "appendFile.txt" (escreveItem ++ "\n") 