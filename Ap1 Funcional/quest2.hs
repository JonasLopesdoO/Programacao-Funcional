--  quest 2

-- funcoes de apoio
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximo de uma lista vazia não existe"
maximum' [x] = x
maximum' (x:xs)
    | x > maxFim = x
    | otherwise = maxFim
    where maxFim = maximum' xs
--COMPLETE AQUI

-- funcao de ordenacao
sort :: [Int] -> [Int] 
sort [] = []
sort [x] = [x]
sort (x:xs) 
    | ultimo > maximum' (init (x:xs)) = sort (init (x:xs)) ++ [ultimo] 
    | otherwise  -- nao consegui finalizar a parte do otherwise 
    where i = n-1
          n = length' (x:xs)
          ultimo = last (x:xs)

