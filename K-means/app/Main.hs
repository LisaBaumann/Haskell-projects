import System.Environment
import System.Exit
import Text.Read
import Data.List
import Data.Char
import Add
import Maxbydistance
import ArgsError
import ClusterMean


closest :: [TupleRGB] -> TupleRGB -> TupleRGB
closest tab y = tab !! (findLeastInt (startCalc tab y []) (minimum (startCalc tab y [])) 0)


getIntFromTuple :: (Int, Float, String) -> Int
getIntFromTuple (a, b, c) = a

getFloatFromTuple :: (Int, Float, String) -> Float
getFloatFromTuple (a, b, c) = b

getStringFromTuple :: (Int, Float, String) -> String
getStringFromTuple (a, b, c) = c

openFile :: String -> IO String
openFile xs = readFile xs

--                   tab       nbCluster    empty
chooseCentroids :: [TupleRGB] -> Int -> [TupleRGB] -> [TupleRGB]
chooseCentroids tab _ _ | (length tab) == 1 = tab
chooseCentroids _ 0 newTab = newTab
chooseCentroids tab nb newTab = chooseCentroids tab (nb - 1) (addTuple (tab !! nb) newTab)

isInList :: TupleRGB -> [TupleRGB] -> Bool
isInList point [] = False
isInList point (x:emptyList)
    | point == x = True
    | otherwise = (isInList point emptyList)

removeDoubles :: TupleRGB -> [TupleRGB] -> [TupleRGB]
removeDoubles point emptyList
    | (isInList point emptyList) == False = (addTuple point emptyList)
    | otherwise = emptyList


removeDoublesLoop :: [TupleRGB] -> [TupleRGB] -> [TupleRGB]
removeDoublesLoop [] emptyList = emptyList
removeDoublesLoop (x:pointsList) emptyList = removeDoublesLoop pointsList (removeDoubles x emptyList)


--        nbCentroid   tab       doubleTab empty
startAlgo :: Int -> [TupleRGB] -> [[TupleRGB]] -> IO ()
startAlgo nb tab (x:doubleTab) = print (addTuple (tab !! 0) x)

cutLine :: String -> String -> String
cutLine [] y = y
cutLine (x:xs) y
    | x == ' ' = xs
    | otherwise = cutLine xs ""

removeFirstColumn :: [String] -> [String] -> [String]
removeFirstColumn [] y  = y
removeFirstColumn (x:xs) y = removeFirstColumn xs (addTab (cutLine x "") y)

cutLineFirst :: String -> String -> String
cutLineFirst [] y = y
cutLineFirst (' ':xs) y = y
cutLineFirst (x:xs) y = cutLineFirst xs (addString x y)

removeSecondColumn :: [String] -> [String] -> [String]
removeSecondColumn [] y = y
removeSecondColumn (x:xs) y = removeSecondColumn xs (addTab (cutLineFirst x "") y)

createListOfList :: [TupleRGB] -> [[TupleRGB]] -> [[TupleRGB]]
createListOfList [] y = tail y
createListOfList (x:xs) y = createListOfList xs (addListTuple [x] y)

--         posInTab   0         ?            listlist        empty
modifyList :: Int -> Int -> [TupleRGB] -> [[TupleRGB]] -> [[TupleRGB]] -> Int -> [[TupleRGB]]
modifyList id i a b c nb 
    | i == nb = tail c
modifyList id i a (b:bs) c nb
    | i == id = modifyList id (i+1) a bs (addListTuple a c) nb
    | otherwise = modifyList id (i+1) a bs (addListTuple b c) nb

--                closest   point to add    listlist       0   nb de cluster
dispatchPoints :: TupleRGB -> TupleRGB -> [[TupleRGB]] -> Int -> Int -> [[TupleRGB]]
dispatchPoints a b c d nb
    | d == nb = c
dispatchPoints a b c d nb
    | a == ((c !! d) !! 0) = modifyList d 0 (addTuple b (c !! d)) c [[]] nb
    | otherwise = dispatchPoints a b c (d + 1) nb

--                    nb    centroid list points list     list list
loopDispatchPoints :: Int -> [TupleRGB] -> [TupleRGB] -> [[TupleRGB]] -> [[TupleRGB]]
loopDispatchPoints nb a [] c = c
loopDispatchPoints nb a (x:xs) c = loopDispatchPoints nb a xs (dispatchPoints (closest a x) x c 0 nb)

loopCalcAverage :: [[TupleRGB]] -> [TupleRGB] -> Int -> [TupleRGB]
loopCalcAverage a b c
    | c == length a = b
loopCalcAverage a b c = loopCalcAverage a (addTuple (getMeanVectorOfCluster (length (a !! c)) (a !! c) (0, 0, 0)) b) (c + 1)

--                                                id  nb reach converg
reachConvergence :: [[TupleRGB]] -> [TupleRGB] -> Int -> Int -> Int -> Int
reachConvergence a b c d e
    | c == (length a) = d
reachConvergence a b c d e
    | (distance ((a !! c) !! 0) (b !! c)) <= e = reachConvergence a b (c + 1) (d + 1) e
    | otherwise = reachConvergence a b (c + 1) d e


getCentroidsList :: [[TupleRGB]] -> [TupleRGB] -> [TupleRGB]
getCentroidsList [] y = y
getCentroidsList (x:xs) y = getCentroidsList xs (addTuple (x !! 0) y)


--             points list    point      position
findPosition :: [TupleRGB] -> TupleRGB -> [String] -> Int -> IO()
findPosition (x:xs) y z i
    | x == y = putStr (z !! i) >> putStr " "
    | otherwise = findPosition xs y z (i + 1)


printPoints :: [TupleRGB] -> Int -> [TupleRGB] -> [String] -> IO()
printPoints [] y _ _ = putStr ""
printPoints (x:xs) 0 pointsList pointsPosition = printPoints xs 1 pointsList pointsPosition
printPoints (x:xs) y pointsList pointsPosition
    | otherwise = findPosition pointsList x pointsPosition 0 >> print x >> printPoints xs (y + 1) pointsList pointsPosition


printFinal :: [[TupleRGB]] -> [TupleRGB] -> [String] -> IO()
printFinal [] _ _ = putStr ""
printFinal (x:xs) pointsList pointsPosition = putStrLn "--" >> print (x !! 0) >> putStrLn "-" >> printPoints x 0 pointsList pointsPosition >> printFinal xs pointsList pointsPosition


mainLoop :: (Int, Float, String) -> [TupleRGB] -> [TupleRGB] -> [TupleRGB] -> Int -> [String] -> IO()
mainLoop args centroidsList lastCentroidsList pointsList y pointsPosition
    | y /= 0 && (reachConvergence (loopDispatchPoints (getIntFromTuple args) centroidsList pointsList (createListOfList centroidsList [[]])) lastCentroidsList 0 0 (getIntFromTuple args)) == (getIntFromTuple args) = printFinal (loopDispatchPoints (getIntFromTuple args) centroidsList pointsList (createListOfList centroidsList [[]])) pointsList pointsPosition
mainLoop args centroidsList lastCentroidsList pointsList y pointsPosition = do
    let pointsWithCentroids = createListOfList centroidsList [[]]
    let final = loopDispatchPoints (getIntFromTuple args) centroidsList pointsList pointsWithCentroids
    mainLoop args (loopCalcAverage final [] 0) (getCentroidsList final []) pointsList (y+1) pointsPosition

removeLastBackline :: String -> IO String
removeLastBackline ('\n':xs) = return (reverse xs)
removeLastBackline xs = return (reverse xs)

checkPointsList :: String -> TupleRGB -> Int -> IO()
checkPointsList b a 1 = putStrLn "--" >> print a >> putStrLn "-" >> putStr b >> putStr " " >> print a >> exitWith ExitSuccess
checkPointsList _ _ _ = return ()

main :: IO()
main = do
    args <- getArgs
    args <- checkLenArgs (length args) >> checkArgs args ((-1), (-1), "")
    file <- openFile (getStringFromTuple args)
    file2 <- (removeLastBackline (reverse (file)))
    let pointsList = (loopTabToTuple (loopGetNbr (loopRemove (removeFirstColumn (updateList (groupBy (\a b -> b /= '\n') file2) []) []) []) []) [])
    let pointsPosition = (removeSecondColumn (updateList (groupBy (\a b -> b /= '\n') file2) []) [])
    checkPointsList (pointsPosition !! 0) (pointsList !! 0) (length pointsList)
    let centroidsList = (chooseCentroids (removeDoublesLoop pointsList []) (getIntFromTuple args) [])
    mainLoop args centroidsList (getCentroidsList (loopDispatchPoints (getIntFromTuple args) centroidsList pointsList (createListOfList centroidsList [[]])) []) pointsList 0 pointsPosition