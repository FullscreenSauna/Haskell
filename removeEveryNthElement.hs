main :: IO ()

removeEveryNthElemnent :: [Integer] -> Int -> [Integer]
removeEveryNthElemnent array n =    
    if null array
        then []
    else   (take (n - 1) array) ++ removeEveryNthElemnent (drop n array) n 


main = do
   print(removeEveryNthElemnent [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 3)