main :: IO ()

-- duplicate array = 
--     if null array
--         then []
--     else head array : head array : (duplicate (tail array))

duplicate array n =
    if null array
        then []
    else (take n (repeat (head array))) ++ (duplicate (tail array) n)

main = do 
    let list = [1,2,3,4,5]
    print (duplicate list 2)
    