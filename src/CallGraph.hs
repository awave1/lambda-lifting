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
type Edge = (String, String)
type EdgeList = [Edge]
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

exploreFunction :: Function -> Graph -> Graph
exploreFunction (Fun (name, args, expr)) graph = newGraph
 where
   -- TODO: move to a function
  graphNewFun = case graph Map.!? name of
    Nothing -> Map.insert name [] graph
  newGraph = exploreFunctionBody expr name graphNewFun

exploreFunctionBody :: Expression -> String -> Graph -> Graph
exploreFunctionBody expr currentFunName graph = case expr of
  LET letFunctions letBody -> do
    let expressionsGraph = exploreFunctionBody letBody currentFunName graph
    let functionsGraph   = exploreFunctions letFunctions expressionsGraph

    functionsGraph

  APP name (exp : exps) -> do
    let adjList      = graph Map.! currentFunName
    let updatedGraph = Map.insert currentFunName (name : adjList) graph

    -- TODO: process expressions

    updatedGraph

  VAR   exp1    -> graph
  CONST exp1    -> graph

  ADD exp1 exp2 -> do
    let cg1       = exploreFunctionBody exp1 currentFunName graph
    let callGraph = exploreFunctionBody exp2 currentFunName cg1

    callGraph

  SUB exp1 exp2 -> do
    let cg1       = exploreFunctionBody exp1 currentFunName graph
    let callGraph = exploreFunctionBody exp2 currentFunName cg1

    callGraph

  MUL exp1 exp2 -> do
    let cg1       = exploreFunctionBody exp1 currentFunName graph
    let callGraph = exploreFunctionBody exp2 currentFunName cg1

    callGraph

  DIV exp1 exp2 -> do
    let cg1       = exploreFunctionBody exp1 currentFunName graph
    let callGraph = exploreFunctionBody exp2 currentFunName cg1

    callGraph

  NEG exp1 -> do
    let callGraph = exploreFunctionBody exp1 currentFunName graph

    callGraph

