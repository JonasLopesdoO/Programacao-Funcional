-- Lista 1 de Programação Funcional

--Questao 1
menorDeDois :: Double -> Double -> Double
menorDeDois x y
            | x > y     = y
            | otherwise = x

--Questao 2
menorDeTres :: Double -> Double -> Double -> Double
menorDeTres x y z
            | x < menorDeDois y z = x
            | y < menorDeDois x z = y   
            | otherwise           = z         

-- Questão 3            
fatorial :: (Integral a) => a -> a
fatorial 0 = 1
fatorial n = n * fatorial(n-1) 

-- Questão 4
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- Questão 5
elemento :: [a] -> Int -> a
elemento u n
    | n > length u - 1 = error "Posição excede o tamanho da lista"
    | otherwise = head(drop n u)

-- Questão 6
pertence :: (Eq a) => a -> [a] -> Bool
pertence x [] = False
pertence x (u:us)
    | x == u    = True
    | otherwise = x `pertence` us

-- Questão 7
total :: (Num b) => [a] -> b
total [] = 0
total (_:xs) = 1 + total xs

-- Questão 8
maior :: (Ord a) => [a] -> a
maior [] = error "Não existe máximo em lista vazia"
maior [x] = x
maior (x:xs)
    | x > maxCauda = x
    | otherwise = maxCauda
    where maxCauda = maior xs 

-- Questão 9
frequencia :: (Num a1, Eq a2) => a2 -> [a2] -> a1
frequencia _ [] = 0
frequencia y (x:xs) = if (y == x) then 1 + frequencia y xs else 0 + frequencia y xs

------------ Questão 10 ------------:
unico :: Eq a2 => a2 -> [a2] -> Bool
unico y x = if ((frequencia y x) == 1) then True else False

------------ Questão 11 ------------:
maioresQue :: Ord a => a -> [a] -> [a]
maioresQue y (x:xs) = [ x | x <- xs, x > y ]

-- Questaõ 12
cat a [] = a
cat [] b = b 
cat a b = cat c (d:b)
        where c = init a 
              d = last a

------------ Questão 13 ------------:
cauda [] = []
cauda [a]= []
cauda (x:xs) = xs

------------ Questão 14 ------------:
corpo :: [a] -> [a]
corpo []     = error "lista vazia"
corpo [a]    = []
corpo a = take ((length a )-1)  a

-- Questão 15
unique [] = []
unique [x] = [x]
unique (y:ys) = if elem y ys 
                    then unique ys
                    else y:unique ys

-- Questão 16
menores y x = [ d | d <- x, d <= y]

-- Questão 17
alter 0 = []
alter a = alter (c)++[a, (-a)]
    where c = a - 1

-- Questão 18
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- Questão 19
divide [] _ = error "Impossível dividir lista vazia."
divide u n  = (take n u, drop n u)
-- quando se coloca entre parentesis, você transforma
-- em uma tupla

-------------- Questão 20 -----------
intercal [] [] = []
intercal x  [] = x
intercal [] y  = y
intercal (x:xs) (y:ys) = x:y:(intercal xs ys)

------------- questão 21 ------------
uniao [] [] = []
uniao [] y  = y
uniao x  [] = x
uniao (x:xs) y = if (elem x y) then uniao xs y else x:uniao xs y

------------ Questão 22 -------------  
intersec [] [] = []
intersec  _ [] = []
intersec []  _ = []
intersec (x:xs) y = if (elem x y) then x:intersec xs y else intersec xs y

----------- Questão 23 --------------
--sequencia 0 m = []
--sequencia n m = [m+a| a <- [0,1..n-1]]
sequencia n m = [m..((m+n)-1)]

----------- Questão 24 --------------
inserir x []=[x]
inserir x (y:ys) = if(x>y) then y:inserir x ys else x:y:ys

----------- Questão 25 --------------
isSorted [] = error "Lista vazia"
isSorted [x]= True
isSorted (x:xs)
    |(x<d) = isSorted xs
    |(x>d) = False
    where d = head xs

-- Questão 26
quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (x:xs) = a ++ [x] ++ b
    where a = quick [n | n <- xs, n < x]
          b = quick [n | n <- xs, n > x ]

----------- Questão 27 ---------------
rotEsq _ [] = []
rotEsq 0 a  = a
rotEsq x (a:as) = rotEsq (x-1) (as++[a])

----------- Questão 28 ---------------
rotDir _ [] = []
rotDir 0 a  = a
rotDir x y  = rotDir (x-1) (ultimo:inicio)
    where ultimo = last y
          inicio = init y 

-- Questão 29 
fnd ch [] = ch
fnd ch (x:xs) 
    | ch == fst x = snd x
    | otherwise = fnd ch xs  

toUpper "" = ""
toUpper (ch:s) = (fnd ch key) : toUpper s 
    where key = zip ['a'..'z'] ['A'..'Z']

-- Questão 30
separa [] = 0
separa (x:xs) = if (x /=' ') then 1 + separa xs else 1

toDow "" = ""
toDow (ch:s) = (fnd ch key) : toDow s 
    where key = zip ['A'..'Z'] ['a'..'z']

upt x = toUpper (take 1 x) ++ toDow (drop 1 x)

titulo [] = []
titulo x = (upt b) ++ (titulo c)
    where a = separa x
          b = take a x
          c = drop a x

-- Questão 31
selec u [] = []
selec [] p = []
selec xs p = [xs!!i | i <- p, i >= 0 && i < length xs]

-- Questão 32
isPalind [] = True
isPalind [a]= True
isPalind x = if (x == contrario) then True else False 
    where contrario = reverso x

----------- Questão 33 ---------------
primo n = not(elem 0 x)
    where x =[mod n a | a<-[2..n-1]]

----------- Questão 34 ---------------
sdig 0 = 0
sdig n = sdig a + b
    where b = mod n 10
          a = div n 10   

-- Questão 35 
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = if sorted thisSort then thisSort else bubbleSort thisSort
    where thisSort = (min x y):bubbleSort ((max x y):xs)  

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False

----------- Questão 36 ---------------
com _ [] = 0
com d (x:xs) = if (x == d) then 1 + (com d xs) else 0

compac [] = []
compac x = if (b > 1) then [[b,c]] ++ compac r else [[c]] ++ compac r 
    where b = com (head x) x
          c = head x
          r = drop b x

----------- Questão 37 ---------------
splitints []  = []
splitints [a] = [[a]]
splitints x = [a] ++ [b]
    where a = [r | r <-x,mod r 2 /=0]
          b = [r | r <-x,mod r 2 ==0]

-- Questão 38
perfeito n = n `elem` m
    where m = [x^2 | x <- [0..n], x^2 == n]

-- Questão 38
perfeito' n = not (null m)
    where m = [x^2 | x <- [0..n], x^2 == n]

-- Questão 39
converte :: Int -> Int -> [Int]
converte 0 _ = []
converte x b = r++[d]
    where r = converte (div x b) b
          d = mod x b
key = ['0'..'9']++['A'..'Z']
--converte x b = [ | q <- l] 
--  where l = base x b
base x b = [key !! q | q <- l]
    where l = converte x b


--lista 2 questao 1
paridade :: [Bool] -> Bool
paridade [] = error "coloque algum elemento na lista"
paridade xs = if odd (length (filter (==True) xs))
              then True
              else False

--lista 2 questao 3
delete' :: (Eq a) => a -> [a] -> [a]
delete' _ [] = []
delete' n lista
    | n `elem` lista = novaLista
    | otherwise = lista
    where novaLista = a ++ b
          a = takeWhile (/=n) lista -- o a não inclui o n
          b = tail $ dropWhile (/=n) lista -- o tail não inclui o n

--lista 2 questao 4 
swap [] _ _ = []
swap [a] _ _ = [a]
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

--------- QUESTÃO 7 -------------------
buscabin [] y     = error "Lista vazia"
buscabin (x:xs) y = if (x /= y) then 1 + (buscabin xs y) else 0  

---------- QUESTÃO 9 -------------------
cc y [] = []
cc y (x:xs) = d:(cc d xs)
    where d = (y + x) 

listacc [] = error "Lista vazia"
listacc x = cc 0 x 
    
