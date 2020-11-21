main = do
    let list = [1,1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,5,7,8]
    print $ foldl (\a b -> 
        if (last a) == b 
            then a 
        else a ++ [b]) [head list] list