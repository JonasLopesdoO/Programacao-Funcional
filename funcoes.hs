p = ["casa", "cachorro", "maca"]
e = ["house", "dog" , "apple"]

[(p!!k)++" "++(e!!k) | k <- [0..2]]

length' l = sum [1 |_ <- l]

removeMaiuscula string = [c | c <- string , not (c `elem` ['A'..'Z'])]

zip [1,2,3] ["um", "dois", "tres"]

triangulo = [(a,b,c) | a <- [1..100], b <- [1..a], c <- [1..b], a + b == c ]
triangulo = [(a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2 ]

triangulo = [ (a,b,c)| c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c <= 1000]

triangulo2 = [ (a,b,c)| c <- [1..], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c <= 1000]

length' l = sum [1 |_ <- l]

 triangulo2 = [ (a,b,c)| c <- [1..400], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c <= 400]

 lista = [(1,2),(3,4),(5,6)]
[r | (a,b) <- lista, let r=a+b]

g = (3:) --junta o 3 com uma lista

f = (/20) -- f e algum número divide o número por 20

zipWith (+) [1,2,3] [3,2,1] --soma duas listas
zipWith (-) [1,2,3] [3,2,1] --subtrai duas listas

zipWith (\a b -> (a,b)) [1,2,3] [3,2,1]  
-- cria uma função dentro do próprio argumento
-- começa com '\' os argumentos e a saida depois do '->' 
-- isso se chama função lambda

takeWhile (<100) [1..]
-- obedece a condicao

foldr1 (max) [20, 30]

foldr1 (+) [20, 30]

foldr (*) 1 [1..10000] --fatorial

map' f xs = foldr (\x acc -> f x : acc) [] xs
-- funcao do mapa

map' f xs = foldl (\x acc -> f x acc) 0 xs
map' f xs = foldr (\x acc -> f x acc) 0 xs

map' f xs = foldr (\x acc -> f x : acc) [] xs

map' f xs = foldl (\b a -> b++[f a]) [] xs
-- b é uma lista

mapisll f xs = foldl (\acc a-> [f a]++acc) [] xs




