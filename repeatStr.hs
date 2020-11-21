repeatStr :: [Char] -> Integer -> [Char]
repeatStr str n =
    if n == 0
    then ""
    else str ++ (repeatStr str (n - 1))

main :: IO ()
main = do
    print (repeatStr "Chushka" 5)

