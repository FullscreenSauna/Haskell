main :: IO ()

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

generateFibonacciSequence :: Integer -> [Integer]
generateFibonacciSequence n =
   if n == 0
       then []
    else (generateFibonacciSequence (n - 1)) ++ [fibonacci n]

main = do
    print (generateFibonacciSequence 10)