module Maxbydistance where
import Add


squareRoot :: Int -> Int
squareRoot n = try n where
   try i | i * i > n = try (i - 1) 
         | i * i <= n = i

distance :: TupleRGB -> TupleRGB -> Int
distance (a, b, c) (d, e, f) = squareRoot ((a - d)^2 + (b - e)^2 + (c - f)^2)

removeBackline :: String -> String -> String
removeBackline [] newTuple = newTuple
removeBackline (x:tuple) newTuple
    | x /= '\n' = removeBackline tuple (x:newTuple)
    | otherwise = removeBackline tuple newTuple

updateList :: [String] -> [String] -> [String]
updateList [] newList = reverse newList
updateList (x:list) newList = updateList list (reverse(removeBackline x ""):newList)

findNumber :: String -> String -> (String, Int)
findNumber (',':xs) y = (xs, (stringToInt y))
findNumber (')':xs) y = (xs, (stringToInt y))
findNumber (x:xs) y = findNumber xs (addString x y)

getNumber :: String -> Int -> [Int] -> [Int]
getNumber x 3 y = y
getNumber x i y = getNumber (fst (findNumber x "")) (i+1) (add (snd (findNumber x "")) y)

removeFirst :: String -> String
removeFirst (x:xs) = xs

loopRemove :: [String] -> [String] -> [String]
loopRemove [] y = y
loopRemove (x:xs) y = loopRemove xs (addTab (removeFirst x) y)

loopGetNbr :: [String] -> [[Int]] -> [[Int]]
loopGetNbr [] y = y
loopGetNbr (x:xs) y = loopGetNbr xs (addTabInt (getNumber x 0 []) y)

convertTabToTuple :: [Int] -> TupleRGB
convertTabToTuple [a, b, c] = (a, b, c)

loopTabToTuple :: [[Int]] -> [TupleRGB] -> [TupleRGB]
loopTabToTuple [] y = y
loopTabToTuple (x:xs) y = loopTabToTuple xs (addTuple (convertTabToTuple x) y)

startCalc :: [TupleRGB] -> TupleRGB -> [Int] -> [Int]
startCalc [] y z = z
startCalc (x:xs) y z = startCalc xs y (add (distance x y) z)

findLeastInt :: [Int] -> Int -> Int -> Int
findLeastInt (x:xs) y z
    | x == y = z
    | otherwise = findLeastInt xs y (z + 1)
