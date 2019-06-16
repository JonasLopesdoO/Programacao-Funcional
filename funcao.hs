head' :: [a] -> a
head' [] = error "Lista vazia não tem cabeça"
head' [x] = x
head' (x:_) = x
-- esse '_' é o qualquer cara

soma :: (Num a) => [a] -> a
soma [] = error "Não pode somar uma lista vazia cara!"
soma [x] = x
soma (x:xs) = x + soma xs

fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac(n-1) 

--fatorial modelo 2
factorial :: Integer -> Integer  
factorial n = product [1..n]  

teste :: (Ord a, Num a) => a -> a -> a
teste x y = if y <= 0
            then error "erro"
            else x+y

--up :: Char -> Char
--up "" = "String vazia..."
--incompleto por enquanto

imc :: Double -> Double -> String
imc peso altura
    | imccalc <= magro = "Voce esta abaixo do peso!"
    | imccalc <= normal = "Voce esta no peso normal!"
    | imccalc <= gordo = "Voce esta acima do peso, para nao dizer gordo!"
    | otherwise        = "Emergencia, mude seus habitos, voca esta obeso"
    where   imccalc = peso / (altura^2)
            magro = 18.5
            normal = 25.0
            gordo = 30.0 


--sol [] = error "Erro, 0"
--sol [x] = x
--sol ((x,y):xs) = fst x + snd y + sol xs
--erro nessa

cilindro r h = 
    let base = pi * r * r
        lat = 2 * pi * r
    in 2 * base + lat
-- lat = lateral do cilindo que envolve as duas bases

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea 


maximo xs = case xs of [] -> error "algo"
                       [x] -> x
                       (x:t) -> max x (maximo t)
-- recursividade para o primeiro elemento da lista e o
-- máximo obtido recursivamente para o resto da lista    

-- funcao que transforma char minúsculo em maiúsculo
fnd ch [] = ch
fnd ch (x:xs) 
    | ch == fst x = snd x
    | otherwise = fnd ch xs  

toUpper "" = ""
toUpper (ch:s) = (fnd ch key) : toUpper s 
    where key = zip ['a'..'z'] ['A'..'Z']
    
toLowwer "" = ""
toLowwer (ch:s) = (fnd ch key) : toLowwer s 
    where key = zip ['A'..'Z'] ['a'..'z']

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximo de uma lista vazia não existe"
maximum' [x] = x
maximum' (x:xs)
    | x > maxFim = x
    | otherwise = maxFim
    where maxFim = maximum' xs
-- onde xs é o resto da lista e se não for o x é o
-- máximo do resto da lista

maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum' xs) 
-- forma mais legível de entender

zipar :: [a] -> [b] -> [(a,b)]
zipar _ [] = []
zipar [] _ = []
zipar (x:xs) (y:ys) = (x,y):zipar xs ys
-- esses ultimos ":" concatenam a tupla (x,y) ao zipar de xs com ys

--para qualquer função '_' em qualquer lista com qualquer coisa
-- retorna uma lista vazia
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

--funcao map pega uma funcao e aplica em todos os elementos de uma lista
--mp :: (t -> a) -> [t] -> [a] - cabeçalho geral
mp _ [] = []
mp f (x:xs) = f x:mp f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' c (x:xs)
    | c x = x:filter' c xs
    | otherwise = filter' c xs
-- onde c é a condição do filter

quick :: (Ord a) => [a] -> [a]
quick [] = []
quick (x:xs) = a ++ [x] ++ b
    where a = quick [n | n <- xs, n < x]
          b = quick [n |n <- xs, n > x ]

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x < y = x:( merge xs (y:ys))
    | otherwise = y:(merge ys (x:xs))

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort (x:[]) = [x]
mergesort xs = merge a b
    where n = (length xs) `div` 2
          a = mergesort (take n xs)
          b = mergesort (drop n xs)
          --a = mergesort $ take n xs
          --b = mergesort $ drop n xs
          --usando o '$' você abole o uso dos parenteses

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs) = if sorted thisSort then thisSort else bubbleSort thisSort
    where thisSort = (min x y) : bubbleSort ((max x y):xs)

sorted :: (Ord a) => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:xs) = if x <= y then sorted (y:xs) else False


-- quest 2 ap

sort :: [Int] -> [Int] 
sort [] = []
sort [a] = [a]
sort xs = sort (a ++ b) ++ [x] 
    where x = maximum xs
          a = takeWhile (/=x) xs -- o a não inclui o x
          b = tail $ dropWhile (/=x) xs -- o tail não inclui o x

--insertion sort
inserir :: (Ord a) => a -> [a] -> [a]
inserir x [] = [x]
inserir x (y:ys) | x < y = x : y : ys
                 | otherwise = y : inserir x ys
    
ordinsercao :: (Ord a) => [a] -> [a]
ordinsercao []     = []
ordinsercao (x:xs) = inserir x (ordinsercao xs)

movmin::[Int] -> [Int] -> [Int]
movmin [x] xs = x:xs
movmin (x:y:xs) rs | x < y = movmin (x:xs) (y:rs)
                   | otherwise = movmin (y:xs) (x:rs)

selsort :: [Int] -> [Int]
selsort [] = []
selsort xs = x:(selsort ys)
    where (x:ys) = movmin xs []

-- primeiro parâmetro: vetor com a sequência de números
-- segundo parametro: número procurado
-- terceiro parametro - limite inferior
-- quarto parametro - limite superior
binSearch :: [Int] -> Int -> Int -> Int -> Int
binSearch vet num lim_inf lim_sup
            | vet!!meio > num = binSearch vet num lim_inf meio
            | vet!!meio < num = binSearch vet num meio lim_sup
            | otherwise = meio
            where meio = div (lim_inf + lim_sup) 2


isok xs x = not (elem 0 m)
        where m = [a mod x | a <- xs]
-- isso retorna os restos das divisoes por 0
-- a função retorna true se o número não é divisível por nenhum anterior
-- ou seja, ele é um novo primo
--next [] 2 = [2]
--next xs x = if  (isok xs x) then
--            next (xs++[x]) (x+1)
--            else
--                next xs (x+1)
--primos n 
  --  take n next [] 2

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs 

capital :: String -> String  
capital "" = "String vazia, oops!"  
capital (x:xs) = "A primeira letra de " ++ (x:xs) ++ " é " ++ [x]

max' :: (Ord a) => a -> a -> a  
max' a b
    | a > b = a
    | otherwise = b

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' 0 _  = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

subsum u i j = sum [u !! k | k <- [i..j]]

comp (i1, j1, s1) (i2, j2, s2)
    | s1 > s2 = (i1,j1,s1)
    | otherwise = (i2,j2,s2) -- avalia qual tupla é maior

subseq :: [Int] -> [Int]
subseq xs = [xs !! k | k <- [i..j] ]
    where n = length  xs
          t = [ (i, j, subsum xs i j) | i <- [0..n-1], j <- [i..n-1]]
          -- t é uma lista de tuplas
          (i, j, _) = foldl1 comp t       

data Point = Point Float Float deriving (Show)  

data Shape = Circle Float Float Float | Rectangle Float Float Float Float   
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- let x i = -1 + 2 * (i - 1)/99
--nome = [Point (x i) ((x i)^2) | i <- [0..100]]
--algo = [Point x y | x <- [1..10], y <- [1..10] ]
--map f [1,5,10]

-- quest 3 ap

data Pessoa = Pessoa String String Int deriving (Show, Eq, Ord)


   