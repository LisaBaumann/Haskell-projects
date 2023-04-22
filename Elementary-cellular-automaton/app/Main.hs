import System.Environment
import ErrorHandling

displayWindowChar :: String -> Int -> Int -> IO()
displayWindowChar x 0 z = return()
displayWindowChar x y z = putStr [(x !! z)] >> displayWindowChar x (y - 1) (z + 1)

wolframRules :: Int -> IO [Int]
wolframRules rule = return [(rule `div` 2^i) `mod` 2 | i <- [0..7]]

patternComparison :: Char -> Char -> Char -> [Int] -> Int
patternComparison '*' '*' '*' y = (y !! 7)
patternComparison '*' '*' ' ' y = (y !! 6)
patternComparison '*' ' ' '*' y = (y !! 5)
patternComparison '*' ' ' ' ' y = (y !! 4)
patternComparison ' ' '*' '*' y = (y !! 3)
patternComparison ' ' '*' ' ' y = (y !! 2)
patternComparison ' ' ' ' '*' y = (y !! 1)
patternComparison ' ' ' ' ' ' y = (y !! 0)

transformIntToChar :: Int -> String
transformIntToChar 0 = " "
transformIntToChar 1 = "*"

-- indice / new line / old line / binaire
startAlgo :: Int -> String -> String -> [Int] -> String
startAlgo a b c d
    | a == (Prelude.length(c) - 1) = b
    | otherwise = startAlgo (a + 1) (b ++ (transformIntToChar (patternComparison (c !! (a - 1)) (c !! a) (c !! (a + 1)) d))) c d

putMove :: Int -> String -> String
putMove 0 y = y
putMove x y = putMove (x - 1) (y ++ " ")

-- string correspond à la première ligne / y correspond au nombre en binaire
wolfram :: String -> [Int] -> [Int] -> IO()
wolfram x [rule, start, 0, window, move] y = return ()
wolfram x [rule, start, lines, window, move] y
    | start <= 0 = (putStrLn ((putMove move "") ++ (take window x))) >> wolfram (startAlgo 1 " " (x ++ " ") y) [rule, start, lines - 1, window, move] y
    | otherwise = wolfram (startAlgo 1 " " (x ++ " ") y) [rule, start - 1, lines, window, move] y

-- utile à la création de la première ligne uniquement
makeSpacesLine :: Int -> String -> String
makeSpacesLine 0 y = y
makeSpacesLine x y = makeSpacesLine (x - 1) (y ++ " ")

createFirstLine :: Int -> String
createFirstLine x 
    | x `mod` 2 == 0 = (makeSpacesLine (x  `div` 2) "") ++ "*" ++ (makeSpacesLine (x `div` 2 - 1) "") ++ " "
    | x `mod` 2 == 1 = (makeSpacesLine (x `div` 2) "") ++ "*" ++ (makeSpacesLine (x `div` 2 ) "") ++ " "

main :: IO()
main = do
    let optionsValues = [-1, 0, 9999999999999999, 80, 0]
    args <- getArgs
    options <- checkValueArg args optionsValues
    checkNbArg args
    checkIsRule (options !! 0)
    rules <- wolframRules (options !! 0)
    wolfram (createFirstLine (options !! 3)) options rules