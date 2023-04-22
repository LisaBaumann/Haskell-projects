module Add where

type TupleRGB = (Int, Int, Int)

stringToInt :: String -> Int
stringToInt arg = read arg :: Int

add :: Int -> [Int] -> [Int] 
add num xs = xs ++ [num]

addString :: Char -> String -> String
addString num xs = xs ++ [num]

addTab :: String -> [String] -> [String]
addTab num xs = xs ++ [num]

addTabInt :: [Int] -> [[Int]] -> [[Int]]
addTabInt num xs = xs ++ [num]

addTuple :: TupleRGB -> [TupleRGB] -> [TupleRGB]
addTuple num xs = xs ++ [num]

addListTuple :: [TupleRGB] -> [[TupleRGB]] -> [[TupleRGB]]
addListTuple num xs = xs ++ [num]