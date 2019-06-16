--data Pessoa = Pessoa String String Int deriving (Show, Eq)
data Pessoa = Pessoa String String Int deriving (Show, Eq, Ord)

data Lista a = Vazia | Nodo a (Lista a) deriving (Show)

add Vazia x = Nodo x Vazia
add ls x = Nodo x ls

addFinal Vazia x = Nodo x Vazia
addFinal (Nodo y l) x = Nodo x (add l y) 

addFinal' [] l = l
addFinal' (x:xs) l = addFinal' xs (add l x)

addList [] = Vazia
addList (x:xs) = Nodo x (addList xs)

lenList Vazia = 0
lenList (Nodo x ls) = 1 + lenList ls

sumList Vazia = 0
sumList (Nodo x ls) = x + sumList ls

rev Vazia = Vazia
rev (Nodo x ls) = Nodo x (rev ls)

--ultElem Vazia = Vazia
--ultElem (Node x Vazia) = Node x
--ultElem (Node x ls) = ultelem ls