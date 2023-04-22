module ClusterMean where
import Add

-- Get new centroid of cluster

getFirstElementOfTuple :: (a, a, a) -> a
getFirstElementOfTuple (a, b, c) = a

getSecondElementOfTuple :: (a, a, a) -> a
getSecondElementOfTuple (a, b, c) = b

getThirdElementOfTuple :: (a, a, a) -> a
getThirdElementOfTuple (a, b, c) = c

addElems :: Int -> Int -> Int
addElems a b = (b + a)

--          LenTuple    MeanTuple
divideTuple :: Int -> TupleRGB -> TupleRGB
divideTuple y (a, b, c) = ((a `div` y), (b `div` y), (c `div` y))

--                       Length         Tab                Empty
getMeanVectorOfCluster :: Int -> [TupleRGB] -> TupleRGB -> TupleRGB
getMeanVectorOfCluster t [] z = (divideTuple t z)
getMeanVectorOfCluster t (y:xs) (a, b, c) = getMeanVectorOfCluster t xs ((addElems (getFirstElementOfTuple y) a), (addElems (getSecondElementOfTuple y) b), (addElems (getThirdElementOfTuple y) c))
