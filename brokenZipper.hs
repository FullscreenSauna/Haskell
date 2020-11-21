main = do
    let firstList = [23, -6, 48, 54, 12, -5]
    let secondList = [15, -3, 55, 3, -4, 6]
    print ( map(\(x,y) -> 
        if x >= 0 && y >= 0
            then x + y
        else 0) 
        (zip firstList secondList))