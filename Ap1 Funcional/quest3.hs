-- quest 3

--funcoes de apio
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximo de uma lista vazia não existe"
maximum' [x] = x
maximum' (x:xs)
    | x > maxFim = x
    | otherwise = maxFim
    where maxFim = maximum' xs

--COMPLETE AQUI

-- funcao subsequencia de soma maxima
subseq :: [Int] -> [Int]
subseq [] = []
subseq [x] = [x]
subseq (x:xs) 
    |maximum' xs > maximum' (x:xs) = xs
    |otherwise subseq xs
--COMPLETE AQUI


