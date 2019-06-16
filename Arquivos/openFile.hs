import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
    -- hGetContents pega o conteúdo de contents
    -- hGetClose fecha handle, que foi aberto o arquivo
