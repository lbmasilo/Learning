module DataStructure
(
 Point(..),
 Node(..),
 Graph(..),
 DataSet(..)
) where

type Point = (Double, Double)
type Node = (Point, [(Double, Point)])
data Graph = Graph [Node] | EmptyGraph
data DataSet = DataSet {degree :: Int, points :: [Point]} deriving (Show, Read)