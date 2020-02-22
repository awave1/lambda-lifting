module AST where

newtype Prog a b = Prog [Fun a b] deriving Show
newtype Fun a b = Fun (a, [b], Exp a b) deriving Show

data BoolExp a b = Lt (Exp a b) (Exp a b)
                | Gt (Exp a b) (Exp a b)
                | Eq (Exp a b) (Exp a b)
                | AND (BoolExp a b) (BoolExp a b)
                | OR (BoolExp a b) (BoolExp a b)
                | NOT (BoolExp a b)
                deriving Show

data Exp a b = ADD (Exp a b) (Exp a b)
             | SUB (Exp a b) (Exp a b)
             | MUL (Exp a b) (Exp a b)
             | DIV (Exp a b) (Exp a b)
             | NEG (Exp a b)
             | CONST Int
             | VAR b
             | COND (BoolExp a b) (Exp a b) (Exp a b)
             | APP a [Exp a b]
             | LET [Fun a b] (Exp a b)
             deriving Show

-- -- Pretty Printer
class Printer a where
    printer:: a -> String

instance Printer a => Printer [a] where
  printer []       = []
  printer (a : as) = printer a ++ printer as

instance Printer Int where
  printer n = "v" ++ show (n :: Int)

instance Printer Char where
  printer c = [c]

-- to print n printSpaces
printSpaces :: Int -> String
printSpaces 0 = ""
printSpaces n = " " ++ printSpaces (n - 1)

showProgram :: (Printer a, Printer b) => Prog a b -> String
showProgram (Prog functions) =
  concatMap (\f -> showFunction 0 f ++ "\n") functions

showFunction :: (Printer a, Printer b) => Int -> Fun a b -> String
showFunction n (Fun (functionName, a1 : args, body)) =
  printSpaces n
    ++ "fun "
    ++ printer functionName
    ++ "("
    ++ printer a1
    ++ concatMap (\a -> "," ++ printer a) args
    ++ ") = "
    ++ buildExpression n body
showFunction n (Fun (functionName, [], body)) =
  printSpaces n ++ printer functionName ++ "() = " ++ buildExpression n body

showExpression :: (Printer a, Printer b) => Int -> Exp a b -> String
showExpression n exp = printSpaces n ++ buildExpression n exp

buildExpression :: (Printer a, Printer b) => Int -> Exp a b -> String
buildExpression n exp = case exp of
  (ADD e1 e2) -> buildExpression n e1 ++ "+" ++ buildExpression n e2
  (MUL e1 e2) -> buildExpression n e1 ++ "*" ++ buildExpression n e2
  (DIV e1 e2) -> buildExpression n e1 ++ "/" ++ buildExpression n e2
  (SUB e1 e2) -> buildExpression n e1 ++ "-" ++ buildExpression n e2
  (NEG   e  ) -> "-" ++ buildExpression n e
  (CONST m  ) -> show m
  (VAR   b  ) -> printer b
  (COND b e1 e2) ->
    "(if "
      ++ showBoolExpressionExp n b
      ++ "\\n"
      ++ printSpaces (n + 3)
      ++ "then "
      ++ buildExpression (n + 3) e1
      ++ "\n"
      ++ printSpaces (n + 3)
      ++ "else "
      ++ buildExpression (n + 3) e1
      ++ ")"
  (APP f (e : es)) ->
    printer f
      ++ "("
      ++ buildExpression n e
      ++ concatMap (\x -> "," ++ buildExpression n x) es
      ++ ")"
  (LET [] e) -> buildExpression n e
  (LET (f : fs) e) ->
    "let\n"
      ++ showFunction (n + 3) f
      ++ concatMap (\f -> "\n" ++ showFunction (n + 3) f) fs
      ++ "\n"
      ++ printSpaces n
      ++ "in  "
      ++ buildExpression n e

showBoolExpressionExp :: (Printer a, Printer b) => Int -> BoolExp a b -> String
showBoolExpressionExp n exp = case exp of
  (Lt e1 e2) -> buildExpression n e1 ++ "<" ++ buildExpression n e2
  (Gt e1 e2) -> buildExpression n e1 ++ ">" ++ buildExpression n e2
  (Eq e1 e2) -> buildExpression n e1 ++ "==" ++ buildExpression n e2
  (AND e1 e2) ->
    showBoolExpressionExp n e1 ++ "&&" ++ showBoolExpressionExp n e2
  (OR e1 e2) ->
    showBoolExpressionExp n e1 ++ "||" ++ showBoolExpressionExp n e2
  (NOT e) -> "not(" ++ showBoolExpressionExp n e ++ ")"
