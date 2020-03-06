module LambdaLifting.Lifting where

import           Parser.AST
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           LambdaLifting.Graph

-- |Vargs set - set of function arguments
type ArgVars = Set String

-- |Vfree set - set of free variables inside function body
type FreeVars = Set String

-- |Maps the name of the function to Vargs and Vfree sets
type FunVarTable = Map String (ArgVars, FreeVars)

{-|
  Note: Incomplete

  Performs lambda lifting for a given Program and a call graph.
  A program is assumed to be alpha renamed already and call graph contains alpha renamed entries
  
  The function:
    - builds a table with all Vargs and Vfree for each function
    - updates the Vargs and Vfree sets for each function based on the call graph
-}
lambdaLifting :: Program -> Graph -> Program
lambdaLifting (Prog functions) callGraph = do
  let varTable     = buildVarTable functions Map.empty

  -- TODO
  let updatedTable = updateVarArgs callGraph varTable

  Prog functions

{-|
  Populate the table with function name and corresponding Vargs & Vfree for that function
-}
buildVarTable :: [Function] -> FunVarTable -> FunVarTable
buildVarTable fns table = foldr buildVarTableEntry table fns

{-|
  Add a single table entry to a state table
-}
buildVarTableEntry :: Function -> FunVarTable -> FunVarTable
buildVarTableEntry (Fun (name, args, exp)) table = updatedTable
 where
  newTable     = Map.insert name (Set.fromList args, Set.empty) table
  updatedTable = getFreeVars exp name newTable

{-|
  Find all free variables withing the function body and add them to the Vfree set for
    the given function
-}
getFreeVars :: Expression -> String -> FunVarTable -> FunVarTable
getFreeVars exp name funVarTable = case exp of
  VAR v -> do
    let (args, freeVars) = funVarTable Map.! name
    Map.insert name (args, Set.insert v freeVars) funVarTable

  LET letFuns letExpr -> do
    let letTable = buildVarTable letFuns funVarTable
    getFreeVars letExpr name letTable

  ADD e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  SUB e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  MUL e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  DIV e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  NEG e1    -> getFreeVars e1 name funVarTable
  _         -> funVarTable

updateVarArgs :: Graph -> FunVarTable -> FunVarTable
updateVarArgs callGraph table = do
  let funList = Map.toList callGraph
  updateVarArgs' funList table

  -- fun:(edge:edges)

  {-
    for f in allfunnames
      for f' in callGraph[f]
        insert var free of f' to var arg of f''
        return fun table
  -}

updateVarArgs' :: [(String, AdjList)] -> FunVarTable -> FunVarTable
updateVarArgs' [] table = table
updateVarArgs' (entry : entries) table =
  updateEntry entry $ updateVarArgs' entries table

updateEntry :: (String, AdjList) -> FunVarTable -> FunVarTable
updateEntry (funName, list) table = do
  let (edge : edges) = Set.toList list
  table
