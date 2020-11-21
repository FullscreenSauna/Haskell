-- Distinct Start

distinct array = foldl (\a e -> a ++ if elem e a then [] else [e]) [] array

-- Distinct End

collectionInputLineInnerSorter firstMethod secondMethod
    | length (distinct firstMethod) < length (distinct secondMethod) = -1
    |  length (distinct firstMethod) > length (distinct secondMethod) = 1
    | otherwise = 0

collectionInputLineSorter firstMethod secondMethod
    | length firstMethod < length secondMethod = -1
    | length firstMethod > length secondMethod = 1
    | otherwise = collectionInputLineInnerSorter firstMethod secondMethod

sortArrayInner :: [String] -> Int -> Int -> [String]
sortArrayInner array currentIndex currentInnerIndex
    | currentIndex >= length array = array
    | currentInnerIndex >= length array = sortArrayInner array (currentIndex + 1) 0
    | collectionInputLineSorter (array!!currentIndex) (array!!currentInnerIndex) > 0 = 
       sortArrayInner (swap array currentIndex currentInnerIndex) currentIndex (currentInnerIndex + 1)
    | otherwise = sortArrayInner array currentIndex (currentInnerIndex  + 1)

sortArray array = sortArrayInner array 0 0

swap array index secondIndex
    | index == secondIndex = array
    | index > secondIndex = swap array secondIndex index
    | otherwise = take index array
    ++ [head (drop secondIndex array)] 
    ++ take (secondIndex - index - 1) (tail (drop index array)) 
    ++ [head (drop index array)] 
    ++ tail (drop secondIndex array) 

main = do
    print $ sortArray ["Pesho", "Gosho", "Tosho", "Prakash"]