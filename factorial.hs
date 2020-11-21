main :: IO ()

factorial 0 = 1
factorial n = n * factorial(n - 1)

getFactorialSequence n = 
    if n == 0
        then []
    else (getFactorialSequence (n - 1)) ++ [factorial n]

main = do
    print (getFactorialSequence 10)