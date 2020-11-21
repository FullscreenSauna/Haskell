forLoop i condition step body = 
    if not(condition i)
    then return()
    else do
        body i
        forLoop(i + step) condition step body

main = do
    forLoop 1 (\x -> x < 11) 1 (\x -> print(x))