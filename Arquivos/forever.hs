import Control.Monad
import Data.Char

main = forever $ do
    putStrLn "Escreva algo: "
    l <- getLine
    putStrLn $ map toUpper l

    -- pipe unix 
    -- cat supertxt.txt | ./forever