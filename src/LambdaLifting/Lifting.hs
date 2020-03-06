module LambdaLifting.Lifting
  ( lambdaLifting
  )
where

import           Parser.AST
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           LambdaLifting.Graph

type ArgVars = Set Arg
type FreeVars = Set Expression

type LiftedFunsState = [Function]

type FunVarTable = Map String (ArgVars, FreeVars)


lambdaLifting :: Program -> Graph -> Program
lambdaLifting a _ = do
  Prog []
