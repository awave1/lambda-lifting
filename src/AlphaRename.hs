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

--                    vars                functions      varcount functionCount
type RenameState = (Map String String, Map String String, Int, Int)

alphaRename :: Prog String String -> Prog String String
alphaRename program = Prog modifiedFunctions
 where
  (_, modifiedFunctions) =
    modifyFunctions ((Map.empty, Map.empty, 0, 0), getFunctions program)

getFunctions :: Prog String String -> [Fun String String]
getFunctions (Prog funs) = funs

modifyFunctions
  :: (RenameState, [Fun String String]) -> (RenameState, [Fun String String])
modifyFunctions (progState, []             ) = (progState, [])
modifyFunctions (progState, fun : functions) = (tbl, fcns)
 where
  (st1, fcn ) = modifyFunction (progState, fun)
  (st2, fss ) = modifyFunctions (st1, functions)
  (tbl, fcns) = (st2, fcn : fss)

modifyFunction
  :: (RenameState, Fun String String) -> (RenameState, Fun String String)
modifyFunction (progState, function) = (resultingState, newFunction)
 where
  (st1           , fun1       ) = modifyFunctionName (progState, function)
  (st2           , fun2       ) = modifyFunctionArgs (st1, fun1)
  (resultingState, newFunction) = modifyFunctionExpression' (st2, fun2)

modifyFunctionName
  :: (RenameState, Fun String String) -> (RenameState, Fun String String)
modifyFunctionName ((varMap, funMap, varCount, funCount), Fun (name, args, exp))
  = case name `Map.member` funMap of
    -- if it already has been renamed, get the renamed function and return it
    True -> ((varMap, funMap, varCount, funCount), fun)     where
      (name, alias) = Map.elemAt (Map.findIndex name funMap) funMap
      fun           = Fun (alias, args, exp)
    -- otherwise, rename the function and increment function counter
    False ->
      ((varMap, Map.insert name alias funMap, varCount, funCount + 1), fun)     where
      alias = "f" ++ show funCount
      fun   = Fun (alias, args, exp)

modifyFunctionArgs
  :: (RenameState, Fun String String) -> (RenameState, Fun String String)
modifyFunctionArgs (progState, Fun (name, args, expr)) =
  (newState, Fun (name, newArgs, expr))
  where (newState, newArgs) = modifyFunctionArg progState args

modifyFunctionArg :: RenameState -> [String] -> (RenameState, [String])
modifyFunctionArg progState [] = (progState, [])
modifyFunctionArg (varMap, funMap, varCount, funCount) (arg : args) =
  (s2, a : args') where
  (s1, a) = case arg `Map.member` varMap of
    True -> ((varMap, funMap, varCount, funCount), varMap Map.! arg)
    False ->
      ((Map.insert arg alias varMap, funMap, varCount + 1, funCount), alias)
      where alias = "x" ++ show varCount
  (s2, args') = modifyFunctionArg s1 args

modifyFunctionExpression'
  :: (RenameState, Fun String String) -> (RenameState, Fun String String)
modifyFunctionExpression' (progState, Fun (name, args, exp)) =
  let (newState, newExpr) = modifyFunctionExpression (progState, exp)
  in  (newState, Fun (name, args, newExpr))

modifyFunctionExpression
  :: (RenameState, Exp String String) -> (RenameState, Exp String String)
modifyFunctionExpression ((varMap, funMap, varCount, funCount), expression) =
  case expression of
    (VAR v) -> case varMap Map.!? v of
      Just alias -> ((varMap, funMap, varCount, funCount), VAR alias)
      Nothing    -> error $ "VAR error: " ++ v

    ADD e1 e2 -> (newState, ADD newExp1 newExp2)     where
      (s1, newExp1) =
        modifyFunctionExpression ((varMap, funMap, varCount, funCount), e1)
      (newState, newExp2) = modifyFunctionExpression (s1, e2)

    SUB exp1 exp2 -> (tb2, SUB express1 express2)     where
      (tb1, express1) =
        modifyFunctionExpression ((varMap, funMap, varCount, funCount), exp1)
      (tb2, express2) = modifyFunctionExpression (tb1, exp2)

    MUL exp1 exp2 -> (tb2, MUL express1 express2)     where
      (tb1, express1) =
        modifyFunctionExpression ((varMap, funMap, varCount, funCount), exp1)
      (tb2, express2) = modifyFunctionExpression (tb1, exp2)

    DIV exp1 exp2 -> (tb2, DIV express1 express2)     where
      (tb1, express1) =
        modifyFunctionExpression ((varMap, funMap, varCount, funCount), exp1)
      (tb2, express2) = modifyFunctionExpression (tb1, exp2)

    NEG exp1 -> (tb1, NEG express1)
     where
      (tb1, express1) =
        modifyFunctionExpression ((varMap, funMap, varCount, funCount), exp1)

    CONST exp1           -> ((varMap, funMap, varCount, funCount), CONST exp1)

    COND bexp1 exp1 exp2 -> (tb3, COND bexpress1 express1 express2)     where
      (tb1, bexpress1) =
        modifyBoolExpression ((varMap, funMap, varCount, funCount), bexp1)
      (tb2, express1) = modifyFunctionExpression (tb1, exp1)
      (tb3, express2) = modifyFunctionExpression (tb2, exp2)

    LET funcs exp2 -> (tb3, LET functions expression)     where
      (tb1, functions1) =
        modifyLets ((varMap, funMap, varCount, funCount), funcs)
      (tb2, functions ) = modifyLetFunctions (tb1, functions1)
      (tb3, expression) = modifyFunctionExpression (tb2, exp2)

    APP exp1 exps -> (tb2, APP expression1 expressions)     where
      (tb1, expression1) =
        modifyApp ((varMap, funMap, varCount, funCount), exp1)
      (tb2, expressions) = modifyListOfExpressions (tb1, exps)

modifyBoolExpression
  :: (RenameState, BExp String String) -> (RenameState, BExp String String)
modifyBoolExpression (progState, boolexpr) = case boolexpr of
  TRUE            -> (progState, boolexpr)

  FALSE           -> (progState, boolexpr)

  AND bool1 bool2 -> (newState, AND newBool1 newBool2)   where
    (s1      , newBool1) = modifyBoolExpression (progState, bool1)
    (newState, newBool2) = modifyBoolExpression (s1, bool2)

  OR bool1 bool2 -> (newState, OR newBool1 newBool2)   where
    (s1      , newBool1) = modifyBoolExpression (progState, bool1)
    (newState, newBool2) = modifyBoolExpression (s1, bool2)

  NOT bool1 -> (newState, NOT newBool1)
    where (newState, newBool1) = modifyBoolExpression (progState, bool1)

  Lt bool1 bool2 -> (newState, Lt newBool1 newBool2)   where
    (s1      , newBool1) = modifyFunctionExpression (progState, bool1)
    (newState, newBool2) = modifyFunctionExpression (s1, bool2)

  Gt bool1 bool2 -> (newState, Gt newBool1 newBool2)   where
    (s1      , newBool1) = modifyFunctionExpression (progState, bool1)
    (newState, newBool2) = modifyFunctionExpression (s1, bool2)

  Eq bool1 bool2 -> (newState, Eq newBool1 newBool2)   where
    (s1      , newBool1) = modifyFunctionExpression (progState, bool1)
    (newState, newBool2) = modifyFunctionExpression (s1, bool2)


modifyListOfExpressions
  :: (RenameState, [Exp String String]) -> (RenameState, [Exp String String])
modifyListOfExpressions (table, []       ) = (table, [])
modifyListOfExpressions (table, e : exprs) = (newTable, expressions) where
  (st1     , e1         ) = modifyFunctionExpression (table, e)
  (st2     , exps       ) = modifyListOfExpressions (st1, exprs)
  (newTable, expressions) = (st2, e1 : exps)

modifyLetFunctions
  :: (RenameState, [Fun String String]) -> (RenameState, [Fun String String])
modifyLetFunctions (table, []      ) = (table, [])
modifyLetFunctions (table, l : list) = (tb2, fun1 : funcs) where
  (tb1, fun1 ) = modifyLetFunction (table, l)
  (tb2, funcs) = modifyLetFunctions (tb1, list)
  -- renameLetFunction :: (ST, Fun String String) -> (ST, (Fun String String))
  modifyLetFunction (table, fun) = (st, fun2)   where
    (st1, fun1) = modifyFunctionArgs (table, fun)
    (st , fun2) = modifyFunctionExpression' (st1, fun1)

modifyLets
  :: (RenameState, [Fun String String]) -> (RenameState, [Fun String String])
modifyLets (table, []      ) = (table, [])
modifyLets (table, f : funs) = (newTable, functions) where
  (t1      , fun1     ) = modifyFunctionName (table, f)
  (t2      , funs2    ) = modifyLets (t1, funs)
  (newTable, functions) = (t2, fun1 : funs2)

modifyApp :: (RenameState, String) -> (RenameState, String)
modifyApp ((varMap, funMap, varCount, funCount), app) =
  if app `Map.member` funMap
    then ((varMap, funMap, varCount, funCount), funMap Map.! app)
    else error $ "unknown function" ++ app
