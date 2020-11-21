reduceArray array accumulator body =
    if null array
    then accumulator
    else reduceArray (tail array) (body accumulator (head array)) body

sumNumbers array = reduceArray array 0 (\accumulator element -> accumulator + element)

main = do
    print $ sumNumbers [1, 2, 3, 4, 5]
    