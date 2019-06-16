import System.Environment

startText :: [[Char]] -> [([Char], Int)]
startText (x:xs)

main = do
    args <- getArgs
    content <- readFile (args !! 0)
    let w = words content -- pega as palavras
    