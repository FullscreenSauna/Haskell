readUntilWord :: String -> [String] -> IO [String]
readUntilWord end array = do
    line <- getLine
    if line == end
        then return array
    else readUntilWord end (array ++ [line])
    
readUntil :: String -> IO [String]
readUntil end = readUntilWord end []


main = do 
    lines <- readUntil "chushka"
    print lines