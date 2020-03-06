module LambdaLifting.Lifting where

import           Parser.AST
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           LambdaLifting.Graph

type ArgVars = Set String
type FreeVars = Set String

type LiftedFunsState = [Function]

type FunVarTable = Map String (ArgVars, FreeVars)

-- | TODO
lambdaLifting :: Program -> Graph -> Program
lambdaLifting (Prog functions) _ = do
  let varTable = buildVarTable functions
  -- build call table
  -- update table with args/free vars
  Prog []

buildVarTable :: [Function] -> FunVarTable
buildVarTable fns = buildVarTable' fns Map.empty

buildVarTable' :: [Function] -> FunVarTable -> FunVarTable
buildVarTable' [] table = table
buildVarTable' (f : fns) table =
  buildVarTableEntry f $ buildVarTable' fns table

buildVarTableEntry :: Function -> FunVarTable -> FunVarTable
buildVarTableEntry (Fun (name, args, exp)) table = updatedTable
 where
  newTable     = Map.insert name (Set.fromList args, Set.empty) table
  updatedTable = getFreeVars exp name newTable

getFreeVars :: Expression -> String -> FunVarTable -> FunVarTable
getFreeVars exp name funVarTable = case exp of
  VAR v -> do
    let (args, freeVars) = funVarTable Map.! name
    Map.insert name (args, Set.insert v freeVars) funVarTable

  LET letFuns letExpr -> do
    let letTable = buildVarTable' letFuns funVarTable
    getFreeVars letExpr name letTable

  ADD e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  SUB e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  MUL e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  DIV e1 e2 -> getFreeVars e1 name $ getFreeVars e2 name funVarTable
  NEG e1    -> getFreeVars e1 name funVarTable
  _         -> funVarTable
