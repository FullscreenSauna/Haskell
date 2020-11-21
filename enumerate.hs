enumerate start length =
    if length == 0 
        then []
    else start : enumerate (start + 1) (length - 1)

main = do
    print $ enumerate 6 10