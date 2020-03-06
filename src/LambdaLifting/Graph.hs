module LambdaLifting.Graph where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type AdjList = Set String
type Graph = Map String AdjList
