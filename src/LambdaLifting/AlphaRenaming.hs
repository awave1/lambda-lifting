module LambdaLifting.AlphaRenaming
  ( alphaRename
  )
where

import           ParseProg
import           Parser.AST
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List
import           Data.Maybe

{-|
  State stores (in order):
    1. - Variable map - varmap[String] = String
         keys are original names
         values are alpha renamed variable names
       - variable count, integer that contains current var count
    2. - Function map - funmap[String] = String
         keys are original names
         values are alpha renamed variable names
       - function counter - integer, that contains current function count.
-}
type RenameState = ((Map String String, Int), (Map String String, Int))

{-|
  Perform alpha renaming for a specified program and return modified Program
-}
alphaRename :: Program -> Program
alphaRename (Prog functions) = do
  -- ^initialize the state for the alpha renaming
   let initialState = ((Map.empty, 0), (Map.empty, 0))
   let (_, modifiedFunctions) = modifyFunctions (initialState, functions)

   Prog modifiedFunctions

{-|
  Modify list of functions.
  Takes in a tuple with state and list of functions to modify
-}
modifyFunctions :: (RenameState, [Function]) -> (RenameState, [Function])
modifyFunctions (progState, []) = (progState, [])
modifyFunctions (progState, fun : functions) =
  (newState, renamedFunction : renamedFunctions)
 where
  (funState, renamedFunction ) = modifyFunction (progState, fun)
  (newState, renamedFunctions) = modifyFunctions (funState, functions)
  -- ^modifyFunction :: (RenameState, Function) -> (RenameState, Function)
  modifyFunction = modifyFunctionBody . modifyFunctionArgs . modifyFunctionName

{-|
  Rename a single function.
  Look up if a function name has been updated already (it would exist in the funMap inside the state)
    if it has, return a new function with a new name and untouched state
    otherwise, create a new alias for a function and update state & return a new function
-}
modifyFunctionName :: (RenameState, Function) -> (RenameState, Function)
modifyFunctionName (((varMap, varCount), (funMap, funCount)), Fun (name, args, expression))
  = case funMap Map.!? name of
    Just alias -> do
      let fun = Fun (alias, args, expression)
      (((varMap, varCount), (funMap, funCount)), fun)
    Nothing -> do
      let alias        = "f" ++ show funCount
      let nextFunCount = funCount + 1
      let fun          = Fun (alias, args, expression)
      (((varMap, varCount), (Map.insert name alias funMap, nextFunCount)), fun)

{-|
  Modify names of function arguments.
  Works the same way as renaming a single variable
-}
modifyFunctionArgs :: (RenameState, Function) -> (RenameState, Function)
modifyFunctionArgs (progState, Fun (name, args, expr)) =
  (newState, Fun (name, newArgs, expr))
 where
  (newState, newArgs) = modifyFunctionArg progState args

  -- ^modifyFunctionArg :: RenameState -> FunArgs -> (RenameState, FunArgs)
  modifyFunctionArg progState [] = (progState, [])
  modifyFunctionArg ((varMap, varCount), (funMap, funCount)) (arg : args) =
    (newState, newArg : newArgs)   where
    (updatedState, newArg) = case varMap Map.!? arg of
      Just alias -> (((varMap, varCount), (funMap, funCount)), alias)
      Nothing    -> do
        let alias        = "x" ++ show varCount
        let nextVarCount = varCount + 1
        (((Map.insert arg alias varMap, nextVarCount), (funMap, funCount)), alias)
    (newState, newArgs) = modifyFunctionArg updatedState args

{-|
  Modify the body of a given function, which in fact just calls modifyExpression
-}
modifyFunctionBody :: (RenameState, Function) -> (RenameState, Function)
modifyFunctionBody (progState, Fun (name, args, exp)) =
  let (newState, newExpr) = modifyExpression (progState, exp)
  in  (newState, Fun (name, args, newExpr))

{-|
  Modify an expression.
  Using case ... of, find the current expression and modify it accordingly
-}
modifyExpression :: (RenameState, Expression) -> (RenameState, Expression)
modifyExpression (((varMap, varCount), (funMap, funCount)), expression) =
  case expression of
    (VAR v) -> case varMap Map.!? v of
      Just alias -> (((varMap, varCount), (funMap, funCount)), VAR alias)
      -- ^unknown variable, display an error
      Nothing    -> error $ "unknown variable name: " ++ v

    ADD e1 e2  -> do
      let (stateE1, newExp1) =
            modifyExpression (((varMap, varCount), (funMap, funCount)), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, ADD newExp1 newExp2)

    SUB e1 e2 -> do
      let (stateE1, newExp1) =
            modifyExpression (((varMap, varCount), (funMap, funCount)), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, SUB newExp1 newExp2)

    MUL e1 e2 -> do
      let (stateE1, newExp1) =
            modifyExpression (((varMap, varCount), (funMap, funCount)), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, MUL newExp1 newExp2)

    DIV e1 e2 -> do
      let (stateE1, newExp1) =
            modifyExpression (((varMap, varCount), (funMap, funCount)), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, DIV newExp1 newExp2)

    NEG e1 -> do
      let (newState, newExp1) =
            modifyExpression (((varMap, varCount), (funMap, funCount)), e1)

      (newState, NEG newExp1)

    COND boolExp e1 e2 -> do
      let (boolExpState, newBoolExp) =
            modifyBoolExpression (((varMap, varCount), (funMap, funCount)), boolExp)
      let (stateE1, newExp1)  = modifyExpression (boolExpState, e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, COND newBoolExp newExp1 newExp2)

    LET functions letExpression -> do
      let (funcState, newFunctions) =
            modifyFunctions (((varMap, varCount), (funMap, funCount)), functions)
      let (newState, newExpression) =
            modifyExpression (funcState, letExpression)

      (newState, LET newFunctions newExpression)

    APP expression expressions -> do
      let (newAppState, newExpression) =
            modifyApp (((varMap, varCount), (funMap, funCount)), expression)
      let (newState, newExpressions) =
            modifyListOfExpressions (newAppState, expressions)

      (newState, APP newExpression newExpressions)

    _ -> (((varMap, varCount), (funMap, funCount)), expression)

{-|
  Modify boolean expression. Works in similar fashion to modifyExpression
-}
modifyBoolExpression
  :: (RenameState, BoolExpression) -> (RenameState, BoolExpression)
modifyBoolExpression (progState, boolexpr) = case boolexpr of
  AND bool1 bool2 -> do
    let (s1, newBool1)       = modifyBoolExpression (progState, bool1)
    let (newState, newBool2) = modifyBoolExpression (s1, bool2)

    (newState, AND newBool1 newBool2)

  OR bool1 bool2 -> do
    let (s1, newBool1)       = modifyBoolExpression (progState, bool1)
    let (newState, newBool2) = modifyBoolExpression (s1, bool2)

    (newState, OR newBool1 newBool2)

  NOT bool1 -> do
    let (newState, newBool1) = modifyBoolExpression (progState, bool1)

    (newState, NOT newBool1)

  Lt bool1 bool2 -> do
    let (s1, newBool1)       = modifyExpression (progState, bool1)
    let (newState, newBool2) = modifyExpression (s1, bool2)

    (newState, Lt newBool1 newBool2)

  Gt bool1 bool2 -> do
    let (s1, newBool1)       = modifyExpression (progState, bool1)
    let (newState, newBool2) = modifyExpression (s1, bool2)

    (newState, Gt newBool1 newBool2)

  Eq bool1 bool2 -> do
    let (s1, newBool1)       = modifyExpression (progState, bool1)
    let (newState, newBool2) = modifyExpression (s1, bool2)

    (newState, Eq newBool1 newBool2)
  
  _            -> (progState, boolexpr)

{-|
  Recursively modify list of expressions
-}
modifyListOfExpressions
  :: (RenameState, [Expression]) -> (RenameState, [Expression])
modifyListOfExpressions (progState, []         ) = (progState, [])
modifyListOfExpressions (progState, exp : exprs) = (newState, newExp : newExps) where
  (expState, newExp ) = modifyExpression (progState, exp)
  (newState, newExps) = modifyListOfExpressions (expState, exprs)

{-|
  Rename a function that is being invoked
-}
modifyApp :: (RenameState, String) -> (RenameState, String)
modifyApp (((varMap, varCount), (funMap, funCount)), app) =
  if app `Map.member` funMap
    then (((varMap, varCount), (funMap, funCount)), funMap Map.! app)
    else error $ "unknown function: " ++ app
