import System.IO  
import System.Directory  
import Data.List  
  
main = do  
    handle <- openFile "todo.txt" ReadMode  
    tempdir <- getTemporaryDirectory  
    (tempName, tempHandle) <- openTempFile tempdir "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStrLn "Essas são as suas tarefas:"  
    putStr $ unlines numberedTasks  
    putStrLn "Qual delas você deseja excluir?"  
    numberString <- getLine  
    let number = read numberString  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"  