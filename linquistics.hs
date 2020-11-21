
-- IsDigit Start

isDigit str =  str `elem` ["0" , "1", "2", "3", "4" , "5", "6", "7","8" , "9"]

-- IsDigit End

-- Distinct Start

distinct array = foldl (\a e -> a ++ if elem e a then [] else [e]) [] array

-- Distinct End

-- Split Start

findNextIndex :: String -> String -> Int -> Int
findNextIndex str spliterator currentIndex 
    | null str = -1
    | take (length spliterator) str == spliterator = currentIndex
    | otherwise = findNextIndex (tail str) spliterator (currentIndex + 1)

splitArray :: String -> String -> [String]
splitArray str spliterator
    | null str = []
    | findNextIndex str spliterator 0 == -1 = [str]
    | otherwise = [take (findNextIndex str spliterator 0) str] ++ splitArray (drop ((findNextIndex str spliterator 0) + (length spliterator)) str) spliterator

split :: String -> String -> [String]
split = splitArray

-- Split End

-- TupleManipulation Start

getElement :: [(String, [String])] -> String -> (String, [String])
getElement tuple key = 
    head (filter (\x -> fst x == key) tuple)

contains :: [(String, [String])] -> String -> Bool
contains tuple key = not $ null $ filter (\x -> fst x == key) tuple 

addElement :: [(String, [String])] -> String -> [String] -> [(String, [String])]
addElement tuple key element 
    | contains tuple key = [(key, distinct $ (snd (getElement tuple key)) ++ element)] 
        ++ filter (\x -> fst x /= key) tuple
    | otherwise = tuple 

-- TupleManipulation End

-- Sort Start

collectionInputLineInnerSorter :: String -> String -> Int
collectionInputLineInnerSorter firstMethod secondMethod
    | length (distinct firstMethod) < length (distinct secondMethod) = -1
    |  length (distinct firstMethod) > length (distinct secondMethod) = 1
    | otherwise = 0

collectionInputLineSorter :: String -> String -> Int
collectionInputLineSorter firstMethod secondMethod
    | length firstMethod < length secondMethod = -1
    | length firstMethod > length secondMethod = 1
    | otherwise = collectionInputLineInnerSorter firstMethod secondMethod

digitInputLineSorter :: (String, [String]) -> (String, [String]) -> Int
digitInputLineSorter firstCollection secondCollection
    | length  (snd firstCollection) < length  (snd secondCollection) = -1
    | length  (snd firstCollection) > length  (snd secondCollection) = 1
    | otherwise = 0

digitInputLineMethodSorter :: String -> String -> Int
digitInputLineMethodSorter firstMethod secondMethod
    | length firstMethod < length secondMethod = 1
    | length firstMethod > length secondMethod = -1
    | otherwise = 0

finaleMethodSorter :: String -> String -> Int
finaleMethodSorter firstMethod secondMethod
    | length firstMethod < length secondMethod = -1
    | length firstMethod > length secondMethod = 1
    | otherwise = 0

finaleCollectionInnerSorter :: (String, [String]) -> (String, [String]) -> Int
finaleCollectionInnerSorter firstCollection secondCollection
    | length  (last (sortArray (snd firstCollection) finaleMethodSorter)) < length  (last (sortArray (snd secondCollection) finaleMethodSorter)) = -1
    | length  (last (sortArray (snd firstCollection) finaleMethodSorter)) > length  (last (sortArray (snd secondCollection) finaleMethodSorter)) = 1
    | otherwise = 0

finaleCollectionSorter :: (String, [String]) -> (String, [String]) -> Int
finaleCollectionSorter firstCollection secondCollection
    | length  (snd firstCollection) < length  (snd secondCollection) = -1
    | length  (snd firstCollection) > length  (snd secondCollection) = 1
    | otherwise = finaleCollectionInnerSorter firstCollection secondCollection

sortArrayInner :: [a] -> (a -> a -> Int) -> Int -> Int -> [a]
sortArrayInner array comparer currentIndex currentInnerIndex
    | currentIndex >= length array = array
    | currentInnerIndex >= length array = sortArrayInner array comparer (currentIndex + 1) 0
    | comparer (array!!currentIndex) (array!!currentInnerIndex) > 0 = 
       sortArrayInner (swap array currentIndex currentInnerIndex) comparer currentIndex (currentInnerIndex + 1)
    | otherwise = sortArrayInner array comparer currentIndex (currentInnerIndex  + 1)

sortArray :: [a]-> (a -> a -> Int) -> [a]
sortArray array comparer = sortArrayInner array comparer 0 0

swap array index secondIndex
    | index == secondIndex = array
    | index > secondIndex = swap array secondIndex index
    | otherwise = take index array
    ++ [head (drop secondIndex array)] 
    ++ take (secondIndex - index - 1) (tail (drop index array)) 
    ++ [head (drop index array)] 
    ++ tail (drop secondIndex array) 

-- Sort End

-- DeleteChars Start

deleteChars :: String -> [Char] -> String
deleteChars str chars
    | null str = []
    | head str `elem` chars = deleteChars (tail str) chars
    | otherwise = [head str] ++ deleteChars (tail str) chars

-- DeleteChars End

processNormalInputLine :: [String] -> [(String, [String])] -> ([(String , [String])], [String])
processNormalInputLine currentParams collections = 
    if contains collections (head currentParams)
        then (addElement collections (head currentParams) (map (\x -> deleteChars x ['(', ')']) (tail currentParams)), [])
    else (collections ++ [(head currentParams , distinct $ map (\x -> deleteChars x ['(', ')']) (tail currentParams))], [])

processCollectionInputLine :: [String] -> [(String, [String])] -> ([(String , [String])], [String])
processCollectionInputLine currentParams collections = 
    if not $ contains collections (head currentParams)
        then (collections, [])
    else (collections, map ("* " ++) (sortArray (snd (getElement collections (head currentParams))) collectionInputLineSorter))

processDigitInputLine :: [String] -> [(String, [String])] -> ([(String , [String])], [String])
processDigitInputLine currentParams collections = 
    (collections, map ("* " ++) (take(read (head currentParams) :: Int) (sortArray (snd (head (sortArray collections digitInputLineSorter))) digitInputLineMethodSorter)))
    

processInputLine :: [String] -> [(String, [String])] -> ([(String , [String])], [String])
processInputLine currentParams collections
    | isDigit $ head currentParams  = processDigitInputLine currentParams collections 
    | null $ tail currentParams =  processCollectionInputLine currentParams collections 
    | otherwise = processNormalInputLine currentParams collections 

printOutput :: [String] -> IO()
printOutput output = 
    if null output
        then return ()
    else do 
        putStrLn $ head output
        printOutput (tail output) 


formatFinaleCollectionAll :: (String, [String]) -> [String]
formatFinaleCollectionAll collection = fst collection : map ("* " ++) (sortArray (snd collection) finaleMethodSorter)

processFinale :: [(String, [String])] -> [String] -> IO()
processFinale collections finalParams = do
    if last finalParams == "collection"
        then do
            printOutput $ map fst (sortArray (filter(\x -> head finalParams `elem` snd x ) collections) digitInputLineSorter)
    else do 
        printOutput $ foldl (\a x -> a ++ x) [] (map formatFinaleCollectionAll (sortArray (filter(\x -> head finalParams `elem` snd x ) collections) finaleCollectionSorter))


finalAction :: [String] -> String -> [(String, [String])] -> IO()
finalAction lines finalLine collections =  do
    if null lines 
        then do 
            let finalParams = split finalLine " "
            processFinale collections finalParams
    else do 
        let currentParams = split (head lines) "."
        let processCollectionsAndOutput = processInputLine currentParams collections
        let processCollections = fst processCollectionsAndOutput
        let processOutput = snd processCollectionsAndOutput

        if not $ null processOutput
            then do 
                printOutput processOutput
                finalAction (tail lines) finalLine processCollections
        else do 
                finalAction (tail lines) finalLine processCollections


readUntil :: [String] -> IO()
readUntil array = do
    line <- getLine
    if line == "exit"
        then do
            finalLine <- getLine
            finalAction array finalLine []
    else readUntil (array ++ [line])
    

main = do
   readUntil []
   
