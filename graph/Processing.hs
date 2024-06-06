module Processing 
(

) where

import DataStructure
import Data.List

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt sumOfSquares
    where
        deltaXs2 = (x1 - x2) ** 2
        deltaYs2 = (y1 - y2) ** 2
        sumOfSquares = deltaXs2 + deltaYs2

calcDistances :: Point -> [Point] -> [(Double, Point)]
calcDistances _ [] = []
calcDistances pt points = zip ds points
    where ds = map calcDist points
          calcDist = distance pt

sortDist :: Ord a1 => (a1, (a2, b1)) -> (a1, (a3, b2)) -> Ordering
sortDist (d1, _) (d2, _)
    | d1 > d2 = GT
    | d1 < d2 = LT
    | d1 == d2 = EQ

findNeighbours :: Int -> [Point] -> Point -> [(Double, Point)]
findNeighbours _ [] _ = []
findNeighbours n points pt = take n sorted 
    where notSelf = filter (/= pt) points
          dists = calcDistances pt notSelf
          sorted = sortBy sortDist dists

buildGraph :: DataSet -> Graph
buildGraph ds
    | null pts = EmptyGraph
    | otherwise = graph
        where d = degree ds
              pts = points ds
              nearestN = findNeighbours d pts
              graph = Graph [(p, nearestN p) | p <- pts]

nodeToString :: Node -> String
nodeToString (source, []) = show source ++ ";"
nodeToString (source, edges) = foldr (++) "" edgeStringList
    where escapeNode n = "\"" ++ show n ++ "\""
          makeEdge d = escapeNode source ++ "->" ++ escapeNode d ++ ";\n"
          edgeStringList = [makeEdge dest | (_, dest) <- edges]

toGraphVizDot :: Graph -> String
toGraphVizDot EmptyGraph = "digraph G {}"
toGraphVizDot (Graph nodes) = let
    nodeToString [] = ""
    nodeToString (n:ns) = nodeToString n ++ nodeToString ns
    in "digraph G {" ++ nodeToString nodes ++ "}"

instance Show Graph where
    show = toGraphVizDot