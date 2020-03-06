module LambdaLifting.CallGraph
  ( buildCallGraph
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Parser.AST
import           LambdaLifting.Graph

{-|
  Takes in a Program and produces a Call Graph, represented as Adjaceny List
-}
buildCallGraph :: Program -> Graph
buildCallGraph (Prog functions) = exploreFunctions functions Map.empty

{-|
  Iterate through the list functions and recursively build a call graph
-}
exploreFunctions :: [Function] -> Graph -> Graph
exploreFunctions []           callGraph = callGraph
exploreFunctions (fun : funs) callGraph = newGraph
 where
  funGraph = exploreFunction fun callGraph
  newGraph = exploreFunctions funs funGraph

{-|
  Insert a new function to a graph and explore the function body (expression)
-}
exploreFunction :: Function -> Graph -> Graph
exploreFunction (Fun (name, args, expr)) graph = newGraph
 where
  graphNewFun = case graph Map.!? name of
    Nothing -> Map.insert name Set.empty graph
  newGraph = exploreExpression expr name graphNewFun

{-|
  Using case ... of find either APP (function invocations) or LET expressions.
  If there's LET expression, recursively explore the expression ("in" part)
    and all the functions in the LET body
  
  If there's an APP (invocation) insert the function that is being invoked into adjacency list
    of the parent function.
  
  The rest of the expressions call exploreExpression recursively
-}
exploreExpression :: Expression -> String -> Graph -> Graph
exploreExpression expr currentFunName graph = case expr of
  LET letFunctions letBody -> do
    let expressionsGraph = exploreExpression letBody currentFunName graph
    let functionsGraph   = exploreFunctions letFunctions expressionsGraph

    functionsGraph

  APP name (exp : exps) -> do
    let adjList = graph Map.! currentFunName
    let updatedGraph =
          Map.insert currentFunName (Set.insert name adjList) graph
    let expGraph = exploreExpression exp currentFunName updatedGraph
    let appGraph = exploreExpressions exps currentFunName expGraph

    appGraph

  ADD exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  SUB exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  MUL exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  DIV exp1 exp2 -> exploreBinExpression exp1 exp2 currentFunName graph
  NEG exp1      -> exploreExpression exp1 currentFunName graph
  _             -> graph

{-|
  explore all of the boolean expressions recursively
-}
exploreBooleanExpression :: BoolExpression -> String -> Graph -> Graph
exploreBooleanExpression boolExpr currentFunName graph = case boolExpr of
  Lt  e1 e2 -> exploreBinExpression e1 e2 currentFunName graph
  Gt  e1 e2 -> exploreBinExpression e1 e2 currentFunName graph
  Eq  e1 e2 -> exploreBinExpression e1 e2 currentFunName graph
  OR  e1 e2 -> exploreBinBoolExpression e1 e2 currentFunName graph
  AND e1 e2 -> exploreBinBoolExpression e1 e2 currentFunName graph
  NOT e1    -> exploreBooleanExpression e1 currentFunName graph
  _         -> graph

{-|
  Explore a single binary expression (ADD, SUB, etc)
-}
exploreBinExpression :: Expression -> Expression -> String -> Graph -> Graph
exploreBinExpression exp1 exp2 currentFunName graph =
  let callGraph = exploreExpression exp2 currentFunName
        $ exploreExpression exp1 currentFunName graph
  in  callGraph

{-|
  Explore a single binary boolean expression (AND, OR, etc)
-}
exploreBinBoolExpression
  :: BoolExpression -> BoolExpression -> String -> Graph -> Graph
exploreBinBoolExpression exp1 exp2 currentFunName graph =
  let callGraph =
          exploreBooleanExpression exp2 currentFunName
            $ exploreBooleanExpression exp1 currentFunName graph
  in  callGraph

{-|
  Recursively explore the list of expressions for a specified function name
-}
exploreExpressions :: [Expression] -> String -> Graph -> Graph
exploreExpressions [] _ g = g
exploreExpressions (e : exprs) currentFunName g =
  exploreExpressions exprs currentFunName $ exploreExpression e currentFunName g
