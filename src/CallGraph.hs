module CallGraph where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           AST

{-
TODO: Build an edge list of functions
-}

-- | An edge is (function name, function name)
type Edge = (String, String)
type Graph = [Edge]
-- type Graph = Map String [String]

-- | f0 -> f1
-- buildCallGraph :: Program -> Graph
-- buildCallGraph (Prog functions) = startExploring functions []

-- startExploring :: [Function] -> Graph -> Graph
-- startExploring (fun : funcs) graph = [(name, "")]
--   where Fun (name, args, expr) = fun
