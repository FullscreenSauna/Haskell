getDigits :: Integer -> [Integer]
getDigits number = map (\x -> read [x] :: Integer) (show number)


isSmaller :: Integer -> Integer -> Integer
isSmaller number currentSmallest
    | number < currentSmallest = 1
    | otherwise = 0

smallest :: [Integer] -> Integer -> Integer
smallest numbers currentSmallest = 
    if null numbers
        then currentSmallest
    else if isSmaller (head numbers) currentSmallest == 1
        then smallest (tail numbers) (head numbers)
    else smallest (tail numbers) currentSmallest


getSmallest :: IO ()
getSmallest = do
    number <- getLine
    let digits = getDigits (read number :: Integer)
    print $ smallest digits 20000

main :: IO ()
main = do
    getSmallest