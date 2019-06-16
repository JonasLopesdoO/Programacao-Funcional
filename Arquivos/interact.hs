main = interact shortLinesOnly

shortLinesOnly :: String -> String  
shortLinesOnly input = 
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result

    -- para testar: cat .\supertxt.txt | .\interact

    -- a função interact pega uma string de entrada, 
    -- transforma ela com uma função, e depois imprime o resultado.