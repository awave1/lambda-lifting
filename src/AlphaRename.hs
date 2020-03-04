module AlphaRename
  ( alphaRename
  )
where

import           ParseProg
import           AST
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List
import           Data.Maybe
import           Control.Monad.State

newtype Identity a = Identity { runIdentity :: a }

{-
    State stores (in order):
        1. Variable map - varmap[String] = String
            keys are original names
            values are alpha renamed variable names
        2. Function map - funmap[String] = String
            keys are original names
            values are alpha renamed variable names
        3. varcount - integer, that contains current variable count.
            Used to rename variables
        4. funcount - integer, that contains current function count.
            Used to rename functions
-}
type RenameState = (Map String String, Map String String, Int, Int)
-- type AlphaState = ((Map String String, Int), (Map String String, Int))

type Program = Prog String String
type Function = Fun String String
type FunArgs = [String]
type Expression = Exp String String
type BoolExpression = BExp String String

alphaRename :: Program -> Program
alphaRename (Prog functions) = Prog modifiedFunctions
 where
  (_, modifiedFunctions) =
    modifyFunctions ((Map.empty, Map.empty, 0, 0), functions)

modifyFunctions :: (RenameState, [Function]) -> (RenameState, [Function])
modifyFunctions (progState, []) = (progState, [])
modifyFunctions (progState, fun : functions) =
  (newState, renamedFunction : renamedFunctions)
 where
  (funState, renamedFunction ) = modifyFunction (progState, fun)
  (newState, renamedFunctions) = modifyFunctions (funState, functions)

modifyFunction :: (RenameState, Function) -> (RenameState, Function)
modifyFunction = modifyFunctionBody . modifyFunctionArgs . modifyFunctionName

modifyFunctionName :: (RenameState, Function) -> (RenameState, Function)
modifyFunctionName ((varMap, funMap, varCount, funCount), Fun (name, args, expression))
  = case funMap Map.!? name of
    Just alias -> do
      let fun = Fun (alias, args, expression)
      ((varMap, funMap, varCount, funCount), fun)
    Nothing -> do
      let alias        = "f" ++ show funCount
      let nextFunCount = funCount + 1
      let fun          = Fun (alias, args, expression)
      ((varMap, Map.insert name alias funMap, varCount, nextFunCount), fun)

-- |
modifyFunctionArgs :: (RenameState, Function) -> (RenameState, Function)
modifyFunctionArgs (progState, Fun (name, args, expr)) =
  (newState, Fun (name, newArgs, expr))
 where
  (newState, newArgs) = modifyFunctionArg progState args
-- modifyFunctionArg :: RenameState -> FunArgs -> (RenameState, FunArgs)
  modifyFunctionArg progState [] = (progState, [])
  modifyFunctionArg (varMap, funMap, varCount, funCount) (arg : args) =
    (newState, newArg : newArgs)   where
    (updatedState, newArg) = case varMap Map.!? arg of
      Just alias -> ((varMap, funMap, varCount, funCount), alias)
      Nothing    -> do
        let alias        = "x" ++ show varCount
        let nextVarCount = varCount + 1
        ((Map.insert arg alias varMap, funMap, nextVarCount, funCount), alias)
    (newState, newArgs) = modifyFunctionArg updatedState args

modifyFunctionBody :: (RenameState, Function) -> (RenameState, Function)
modifyFunctionBody (progState, Fun (name, args, exp)) =
  let (newState, newExpr) = modifyExpression (progState, exp)
  in  (newState, Fun (name, args, newExpr))

modifyExpression :: (RenameState, Expression) -> (RenameState, Expression)
modifyExpression ((varMap, funMap, varCount, funCount), expression) =
  case expression of
    (VAR v) -> case varMap Map.!? v of
      Just alias -> ((varMap, funMap, varCount, funCount), VAR alias)
      Nothing    -> error $ "VAR error: " ++ v

    CONST exp1 -> ((varMap, funMap, varCount, funCount), CONST exp1)

    ADD e1 e2  -> do
      let (stateE1, newExp1) =
            modifyExpression ((varMap, funMap, varCount, funCount), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, ADD newExp1 newExp2)

    SUB e1 e2 -> do
      let (stateE1, newExp1) =
            modifyExpression ((varMap, funMap, varCount, funCount), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, SUB newExp1 newExp2)

    MUL e1 e2 -> do
      let (stateE1, newExp1) =
            modifyExpression ((varMap, funMap, varCount, funCount), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, MUL newExp1 newExp2)

    DIV e1 e2 -> do
      let (stateE1, newExp1) =
            modifyExpression ((varMap, funMap, varCount, funCount), e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, DIV newExp1 newExp2)

    NEG e1 -> do
      let (newState, newExp1) =
            modifyExpression ((varMap, funMap, varCount, funCount), e1)

      (newState, NEG newExp1)

    COND boolExp e1 e2 -> do
      let (boolExpState, newBoolExp) =
            modifyBoolExpression ((varMap, funMap, varCount, funCount), boolExp)
      let (stateE1, newExp1)  = modifyExpression (boolExpState, e1)
      let (newState, newExp2) = modifyExpression (stateE1, e2)

      (newState, COND newBoolExp newExp1 newExp2)

    LET functions letExpression -> do
      let (letState, newLetFunctions) =
            modifyLets ((varMap, funMap, varCount, funCount), functions)
      let (funcState, newFunctions) =
            modifyLetFunctions (letState, newLetFunctions)
      let (newState, newExpression) =
            modifyExpression (funcState, letExpression)

      (newState, LET newFunctions newExpression)

    APP expression expressions -> do
      let (newAppState, newExpression) =
            modifyApp ((varMap, funMap, varCount, funCount), expression)
      let (newState, newExpressions) =
            modifyListOfExpressions (newAppState, expressions)

      (newState, APP newExpression newExpressions)

modifyBoolExpression
  :: (RenameState, BoolExpression) -> (RenameState, BoolExpression)
modifyBoolExpression (progState, boolexpr) = case boolexpr of
  TRUE            -> (progState, boolexpr)

  FALSE           -> (progState, boolexpr)

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

modifyListOfExpressions
  :: (RenameState, [Expression]) -> (RenameState, [Expression])
modifyListOfExpressions (progState, []         ) = (progState, [])
modifyListOfExpressions (progState, exp : exprs) = (newState, newExp : newExps) where
  (expState, newExp ) = modifyExpression (progState, exp)
  (newState, newExps) = modifyListOfExpressions (expState, exprs)

modifyLetFunctions :: (RenameState, [Function]) -> (RenameState, [Function])
modifyLetFunctions (progState, []      ) = (progState, [])
modifyLetFunctions (progState, l : list) = (newState, f : funs) where
  (letState, f   ) = modifyLetFunction (progState, l)
  (newState, funs) = modifyLetFunctions (letState, list)
  -- renameLetFunction :: (ST, Fun String String) -> (ST, (Fun String String))
  modifyLetFunction (progState, fun) = (newState, newFun)   where
    (argsState, newArgsFun) = modifyFunctionArgs (progState, fun)
    (newState , newFun    ) = modifyFunctionBody (argsState, newArgsFun)

modifyLets :: (RenameState, [Function]) -> (RenameState, [Function])
modifyLets (progState, []      ) = (progState, [])
modifyLets (progState, f : funs) = (newState, newFun : newFuns) where
  (newFunState, newFun ) = modifyFunctionName (progState, f)
  (newState   , newFuns) = modifyLets (newFunState, funs)

modifyApp :: (RenameState, String) -> (RenameState, String)
modifyApp ((varMap, funMap, varCount, funCount), app) =
  if app `Map.member` funMap
    then ((varMap, funMap, varCount, funCount), funMap Map.! app)
    else error $ "unknown function: " ++ app
