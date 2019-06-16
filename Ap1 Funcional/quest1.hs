
-- Funcoes de apoio

--COMPLETE AQUI

-- funcao mdc para implemenyar
mdc :: Int -> Int -> Int
mdc 0 0 = error "Ambos não podem ser 0"
mdc a b
    | b == 0 = a
    | otherwise = mdc b (a `mod` b)

--COMPLETE AQUI


