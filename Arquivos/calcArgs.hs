import System.Environment

make :: [Char] -> Double -> Double -> Double

make s x y
    | s == "add" = x+y
    | s == "sub" = x-y
    | s == "mul" = x*y
    | s == "div" = x/y
--    | otherwise = "operador invalido"

main = do
    args <- getArgs
    let n = length args
    if(n == 3)
        then do
                let cmd = (args !! 0)
                    x = read (args !! 1) :: Double
                    y = read (args !! 2) :: Double
                    r = make cmd x y
                putStrLn $ show r
                return()
        else do
            putStrLn "Operação Inválida"
        
-- words x ["alguma","coisa","alguma","coisa"]    
-- "alguma coisa \n alguma coisa"  
-- lines x
-- ["alguma coisa "," alguma coisa"]  
--ghci> mapM print [1,2,3]  
--1  
--2  
--3  
--[(),(),()]  
--ghci> mapM_ print [1,2,3]  
--1  
--2  
--3  