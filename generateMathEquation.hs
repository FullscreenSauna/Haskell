generateMathEquation :: [Integer] -> String
generateMathEquation [] = ""
generateMathEquation array = foldr(\x y -> concat["(",x," + ",y,")"]) (show(last array)) (map show (init array))

generateMathEquationReversed :: [Integer] -> String
generateMathEquationReversed [] = ""
generateMathEquationReversed array = foldl(\x y -> concat["(",x," + ",y,")"]) (show(head array)) (map show (tail  array))


main = do
    let list = [1, 2, 3, 4, 5]
    print $ generateMathEquation list
    print $ generateMathEquationReversed list
    --print $ generateMathEquation 5 6 