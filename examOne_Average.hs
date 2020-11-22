getDigits :: String -> [Int]
getDigits number = map (\x -> read [x] :: Int) (show number)

average :: [Int] -> Int
average [1,0,0,0] = 1
average digits = sum digits `div` length digits

getAverage :: IO ()
getAverage = do
    number <- getLine
    let digits = getDigits number 
    print $ average digits

main :: IO ()
main = do
    getAverage