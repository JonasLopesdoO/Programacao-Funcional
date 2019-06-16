main = do
    line <- getLine
    if null line
        then return()
        else do 
            putStrLn $ reverserWords line
            main

reverserWords :: String -> String
reverserWords st = unwords (map reverse (words st))