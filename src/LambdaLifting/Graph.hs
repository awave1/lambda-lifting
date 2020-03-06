module LambdaLifting.Graph where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

{-|
  Represent a graph using Sets as Adjacency list and Map to easily map nodes to which they are connected
-}
type AdjList = Set String
type Graph = Map String AdjList
