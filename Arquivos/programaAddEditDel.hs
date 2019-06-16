import System.Environment
import System.Directory
import System.IO
import Data.List

comandos :: [(String, [String] -> IO ())]
comandos =  [("add", add),
             ("view", view),
             ("remove", remove)
            ]

main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command comandos  --usa o Maybe para procurar 
    action args                                  -- pela função em comandos
                                                 -- adiciona a action

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    tempdir <- getTemporaryDirectory  
    (tempName, tempHandle) <- openTempFile tempdir "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  

    -- testando:  .\programaAddEditDel.exe view todo.txt
    -- .\programaAddEditDel.exe add todo.txt "Pegar as crianças na lavanderia" 
    -- OBS: SÓ DA PRA TESTAR O REMOVE SE FOR EM ALGUM LUGAR DO DISCO C: