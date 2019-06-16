type Day = Int

data Month = Jan | Fev | Mar | Abr | 
             Mai | Jun | Jul | Ago | 
             Set | Out | Nob | Dez  
             deriving (Eq, Ord, Show, Enum, Bounded) -- O enum define a ordem na ordem 
                                                     -- é definida

type Year = Int

data Date = Date { day   :: Day,
                   month :: Month,
                   year  :: Year }

instance Show Date where
    show (Date d m a) = 
        show d ++ "/" ++
        show m ++ "/" ++ -- para transformar o Jan em 1 e assim por diante
        show a

strToDate str = Date dia (mes) ano
    where dia = read (w !! 0) :: Int
          mes = toEnum ((read (w !! 1)) - 1)
          ano = read (w !! 2) :: Int
          w = words s
          s = [ f ch | ch <- str ]
          f c = if c == '/' then ' ' else c