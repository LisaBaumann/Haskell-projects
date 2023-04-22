module ErrorHandling where
import System.Exit

stringToInt :: String -> Int -- exit erreur si le int ne correspond pas
stringToInt x = read x :: Int

checkNbArg :: [String] -> IO()
checkNbArg [] = putStrLn("Not enough arguments") >> exitWith(ExitFailure 84)
checkNbArg x = return()

checkIsRule :: Int -> IO()
checkIsRule (-1) = putStrLn("Rule invalid") >> exitWith(ExitFailure 84)
checkIsRule x 
    | x >= 0 && x <= 255 = return()
    | otherwise = putStrLn("Rule invalid") >> exitWith(ExitFailure 84)

checkRule :: String -> [Int] -> [Int] -- remplir la case si elle est différente de -1 sinon erreur
checkRule x [rule, start, lines, window, move] = [stringToInt(x), start, lines, window, move]

checkStart :: String -> [Int] -> [Int]
checkStart x [rule, start, lines, window, move] = [rule, stringToInt(x), lines, window, move]

checkLines :: String -> [Int] -> [Int]
checkLines x [rule, start, lines, window, move] = [rule, start, stringToInt(x), window, move]

checkWindow :: String -> [Int] -> [Int]
checkWindow x [rule, start, lines, window, move] = [rule, start, lines, stringToInt(x), move]

checkMove :: String -> [Int] -> [Int]
checkMove x [rule, start, lines, window, move] = [rule, start, lines, window, stringToInt(x)]

checkValueArg :: [String] -> [Int] -> IO [Int]
checkValueArg [] y = return y
checkValueArg (x:xs) y
    | x == "--rule" = checkValueArg xs (checkRule (xs !! 0) y)
    | x == "--start" = checkValueArg xs (checkStart (xs !! 0) y)
    | x == "--lines" = checkValueArg xs (checkLines (xs !! 0) y)
    | x == "--window" = checkValueArg xs (checkWindow (xs !! 0) y)
    | x == "--move" = checkValueArg xs (checkMove (xs !! 0) y)
    | otherwise = checkValueArg xs y