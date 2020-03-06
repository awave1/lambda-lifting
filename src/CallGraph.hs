module CallGraph
  ( buildCallGraph
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           AST

type Graph = Map String [String]

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
  graphNewFun = case graph Map.!? name of
    Nothing -> Map.insert name [] graph
  newGraph = exploreExpression expr name graphNewFun

exploreExpression :: Expression -> String -> Graph -> Graph
exploreExpression expr currentFunName graph = case expr of
  LET letFunctions letBody -> do
    let expressionsGraph = exploreExpression letBody currentFunName graph
    let functionsGraph   = exploreFunctions letFunctions expressionsGraph

    functionsGraph

  APP name (exp : exps) -> do
    let adjList      = graph Map.! currentFunName
    let updatedGraph = Map.insert currentFunName (name : adjList) graph
    let expGraph     = exploreExpression exp currentFunName updatedGraph
    let appGraph     = exploreExpressions exps currentFunName expGraph

    appGraph

  ADD exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  SUB exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  MUL exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  DIV exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  NEG exp1      -> exploreExpression exp1 currentFunName graph
  _             -> graph

exploreBinExpression :: Expression -> Expression -> String -> Graph -> Graph
exploreBinExpression exp1 exp2 currentFunName graph =
  let callGraph = exploreExpression exp2 currentFunName
        $ exploreExpression exp1 currentFunName graph
  in  callGraph

exploreExpressions :: [Expression] -> String -> Graph -> Graph
exploreExpressions [] _ g = g
exploreExpressions (e : exprs) currentFunName g =
  exploreExpressions exprs currentFunName $ exploreExpression e currentFunName g
