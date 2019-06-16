main = do   --putChar 't'  
            --putChar 'e'  
            --putChar 'h'  
              
            c <- getChar  
            if c /= ' '  
                then do  
                    putChar c  
                    main  
                else return () 