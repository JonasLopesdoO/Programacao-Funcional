main = do
    putStrLn "Hello whats your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ " you're amazing!")