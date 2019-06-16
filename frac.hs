module Frac 
( Frac (..)
, mdc
, frac
) where

data Frac = Infinity | Undefined | Load Integer Integer
    
mdc a 0 = a
mdc a b = mdc b (mod a b)  

frac 0 0 = Undefined
frac _ 0 = Infinity
frac a b = Load (sinal*(div a m)) (sinal*(div b m))
    where m = mdc (abs a) (abs b)
          n = div (sinal*a) m
          d = div (abs b) m
          sinal = (if b < 0 then -1 else 1)

instance Show Frac where
    show Infinity =  "oo"
    show Undefined = "</>"
    show (Load a b) = show(a) ++ "/" ++ show(b) -- são dois shows diferente, o esquerdo é um e os direitos são outro
                                                -- o primeiro é o que precisa ser implementado, e o segundo converte algo para string

instance Eq Frac where 
    (==) (Load a b) (Load c d) = (a*d==b*c)
    (==) _ _ = False
    
instance Ord Frac where
    compare (Load a b) (Load c d) 
        | x >  y = GT
        | x == y  = EQ
        | x <  y = LT
        where  x = a*d
               y = b*c 

instance Num Frac where
    (+) (Load a b) (Load c d) = frac (a*d + c*b) (b*d)
    (*) (Load a b) (Load c d) = frac (a*c) (b*d)
    negate (Load a b) = Load (-a) b
    abs (Load a b) = Load (abs a) (abs b)
    signum (Load a b) = (if a*b > 0 then 1 else -1) 
    fromInteger n = Load n 1

--milImpares = [x| x <- [1..1000], odd x]

--fracMil = frac 1 milImpares

fracToReal Infinity = error "real muito grande"
fracToReal Undefined = error "sem representacao numerica"
--fracToReal (Load a b) = a/b


