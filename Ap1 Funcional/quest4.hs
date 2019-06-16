-- quest 4

-- funcoes de apoio
divisao a
    | a == 1 = (a*a) + (1 / divisao b)
    | a > 100 = divisao 100
    where b = a+1
--COMPLETE AQUI

-- calcula pi
getPi :: Fractional a => a
getPi 0 = 0
getPi divisao 1 **(-1) / 4
--COMPLETE AQUI
