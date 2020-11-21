main = do
    let firstList = [25, 43, 11, 28, 44]
    let secondList = [15, -3, 29, 12, -4]
    print ( map (\(x, y) -> x + y) (zip firstList secondList))