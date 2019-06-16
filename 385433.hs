-- NOME: Jonas Lopes do Ó   
-- CURSO: Engenharia de Software
-- MATRICULA: 385433

----------
-- quest 1
----------
--retornaDivisor :: Ord t => t -> t -> t
--retornaDivisor x 0 = 0
--retornaDivisor x n = if x `div` n == 0 then retornaDivisor x (n/10) else n

--lsDig :: Integral t => t -> [t]
--lsDig x = [prim] ++ [restante]
--    where n = 100
--          prim = x `div` n
--          restante = lsDig (x `mod` n)         

isPanDig :: Integral t => t -> Bool
isPanDig 0 = False

lsPanDig :: (Eq a, Integral t) => a -> [t]
lsPanDig _ = []

----------
-- quest 2
----------
--rmFirst :: Eq a => [a] -> a -> [a]
rmFirst [] _  = []
rmFirst l n = prim ++ final
    where prim = takeWhile (<n) l 
          final = dropWhile (<=n) l
          
minMaxSort :: Ord t => [t] -> [t]
minMaxSort [] = []
minMaxSort (x:xs) = [menor]++minMaxSort meio++[maior]
    where menor = minimum (x:xs)
          maior = maximum (x:xs)
          meio = init xs

----------
-- quest 3
----------
--swap :: [a] -> Int -> Int -> [a]
swap [a] _ _  = []
swap u p q = k' ++ bd ++ l'
    where m = if p < q then (u !! p) else (u !! q)
          n = if q < p then (u !! p) else (u !! q)
          k = takeWhile (/=m) u
          l = dropWhile (/=n) u
          k' = k ++ [n]
          v = tail l
          l' = [m] ++ v
          b = dropWhile (/=m) u
          d = takeWhile (/=n) b
          bd = tail d

nextPerm :: Ord a => [a] -> [a]
nextPerm     _ = []

----------
-- quest 4
----------
rmChar :: Eq t => [t] -> t -> [t]
rmChar string c = filter (/=c) string

unique :: Eq t => [t] -> [t]
--unique _ = []
unique [] = []
unique [x] = [x]
unique (y:ys) = if elem y ys 
                    then unique ys
                    else y:unique ys

freqChar :: (Eq a, Num b) => [a] -> [(a, b)]
freqChar [] = []
freqChar (x:xs) = zip listaChar listaNumeros
    where listaChar = unique (x:xs)
          listaNumeros = listaNum listaChar (x:xs)

listaNum [] [] = []
listaNum _ [] = []
listaNum [] _ = []
listaNum (x:xs) (y:ys) = (ocorrencia x (y:ys)): (listaNum xs (rmChar (y:ys) y))
--onde x é a lista de chars únicos de uma string e y é a lista em si

ocorrencia :: (Num a1, Eq a2) => a2 -> [a2] -> a1
ocorrencia _ [] = 0
ocorrencia y (x:xs) = if (y == x) then 1 + ocorrencia y xs else 0 + ocorrencia y xs

