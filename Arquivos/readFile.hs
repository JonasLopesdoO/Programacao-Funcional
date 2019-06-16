import System.IO

main = do
    contents <- readFile "girlFriend.txt"
    putStr contents