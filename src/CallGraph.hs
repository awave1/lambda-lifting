module CallGraph where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           AST

{-
TODO: Build an edge list of functions
-}

-- | An edge is (function name, function name)
-- type Edge = 
type Graph = Map String [String]
-- type Graph = Map String [String]

-- | f0 -> f1
buildCallGraph :: Program -> Graph
buildCallGraph (Prog functions) = exploreFunctions functions Map.empty

exploreFunctions :: [Function] -> Graph -> Graph
exploreFunctions []           callGraph = callGraph
exploreFunctions (fun : funs) callGraph = newGraph
 where
  funGraph = exploreFunction fun callGraph
  newGraph = exploreFunctions funs funGraph

-- TODO: exoplore expression in a function?
exploreFunction :: Function -> Graph -> Graph
exploreFunction (Fun (name, args, expr)) graph = newGraph
 where
  graphNewFun = Map.insert name [] Map.empty
  newGraph    = exploreFunctionBody expr graphNewFun


exploreFunctionBody :: Expression -> Graph -> Graph
exploreFunctionBody = undefined
