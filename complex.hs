data Complex = Complex Float Float 

instance Num Complex where
    (+) (Complex a b) (Complex c d) = Complex (a+c) (b+d)
    (*) (Complex a b) (Complex c d) = Complex (a*c - b*d) (a*d + c*b)
    --fromInteger x = Complex (fromIntegral x 0) 0

instance Show Complex where
   show (Complex a b) = show(a) ++ sig ++ show(b) ++ " i"
                   where sig = if b<0 
                               then " " 
                               else "+"