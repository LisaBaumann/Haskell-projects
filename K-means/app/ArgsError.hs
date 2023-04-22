module ArgsError where
import Add
import System.Exit
import Data.Char

type Arguments = (Int, Float, String)

isNumbeurre :: String -> IO Bool
isNumbeurre [] = return True
isNumbeurre (x:xs)
    | isDigit x == True = isNumbeurre xs
    | otherwise = exitWith (ExitFailure 84)

isFlottant :: String -> IO Bool
isFlottant [] = return True
isFlottant ('.':xs) = isFlottant xs
isFlottant (x:xs)
    | isDigit x == True = isFlottant xs
    | otherwise = exitWith (ExitFailure 84)

stringToFloat :: String -> Float
stringToFloat arg = read arg :: Float

charToString :: Char -> String
charToString c = [c]

checkArgs :: [String] -> Arguments -> IO Arguments
checkArgs [] z = return z
checkArgs (_:[]) z = return z
checkArgs ("-n":y:xs) (a, b, c) = isNumbeurre y >> checkArgs (xs) (stringToInt y, b, c)
checkArgs ("-l":y:xs) (a, b, c) = isFlottant y >> checkArgs (xs) (a, stringToFloat y, c)
checkArgs ("-f":y:xs) (a, b, c) = checkArgs (xs) (a, b, y)
checkArgs (_:y:xs) (a, b, c) = exitWith (ExitFailure 84)

checkLenArgs :: Int -> IO Bool
checkLenArgs 0 = putStr ("USAGE: ./imageCompressor -n N -l L -f F\n\n      N       number of colors in the final image\n      L       convergence limit\n      F       path to the file containing the colors of the pixels\n") >> exitWith (ExitFailure 84)
checkLenArgs 6 = return True
checkLenArgs x = exitWith (ExitFailure 84)