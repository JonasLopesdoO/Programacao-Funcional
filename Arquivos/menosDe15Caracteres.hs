main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input = 
    let allLines = lines input
        shortLines = filter (\line -> length line < 15) allLines
        result = unlines shortLines
    in
        result
        --lines é uma função que quebra em cada \n

        -- para testas: [ cat .\supertxt.txt | .\menosDe15Caracteres ]  