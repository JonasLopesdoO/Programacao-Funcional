-- isso:
--main = do  
--    a <- getLine  
--    b <- getLine  
--    c <- getLine  
--    print [a,b,c]

-- é o mesmo que isso:

main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs