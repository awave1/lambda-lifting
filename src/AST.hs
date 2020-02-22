module AST where

newtype Prog a b = Prog [Fun a b]
newtype Fun a b = Fun (a, [b], Exp a b)

data BoolExp a b = Lt (Exp a b) (Exp a b)
                | Gt (Exp a b) (Exp a b)
                | Eq (Exp a b) (Exp a b)
                | AND (BoolExp a b) (BoolExp a b)
                | OR (BoolExp a b) (BoolExp a b)
                | NOT (BoolExp a b)

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

-- Pretty Printer
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

showProg :: (Printer a, Printer b) => Prog a b -> String
-- showProg (Prog funs) = concat (map (\f -> (showFun 0 f) ++ "\n") funs)
showProg (Prog funs) = concatMap (\f -> showFun 0 f ++ "\n") funs

showFun :: (Printer a, Printer b) => Int -> Fun a b -> String
showFun n (Fun (fname, a1 : args, body)) =
  printSpaces n
    ++ "fun "
    ++ printer fname
    ++ "("
    ++ printer a1
    ++ concatMap (\a -> "," ++ printer a) args
    ++ ") = "
    ++ showExp' n body
showFun n (Fun (fname, [], body)) =
  printSpaces n ++ printer fname ++ "() = " ++ showExp' n body

showExp :: (Printer a, Printer b) => Int -> Exp a b -> String
showExp n exp = printSpaces n ++ showExp' n exp

showExp' :: (Printer a, Printer b) => Int -> Exp a b -> String
showExp' n (ADD e1 e2) = showExp' n e1 ++ "+" ++ showExp' n e2
showExp' n (MUL e1 e2) = showExp' n e1 ++ "*" ++ showExp' n e2
showExp' n (DIV e1 e2) = showExp' n e1 ++ "/" ++ showExp' n e2
showExp' n (SUB e1 e2) = showExp' n e1 ++ "-" ++ showExp' n e2
showExp' n (NEG   e  ) = "-" ++ showExp' n e
showExp' n (CONST m  ) = show m
showExp' n (VAR   b  ) = printer b
showExp' n (COND b e1 e2) =
  "(if "
    ++ showBoolExp n b
    ++ "\\n"
    ++ printSpaces (n + 3)
    ++ "then "
    ++ showExp' (n + 3) e1
    ++ "\n"
    ++ printSpaces (n + 3)
    ++ "else "
    ++ showExp' (n + 3) e1
    ++ ")"
showExp' n (APP f (e : es)) =
  printer f
    ++ "("
    ++ showExp' n e
    ++ concatMap (\x -> "," ++ showExp' n x) es
    ++ ")"
showExp' n (LET [] e) = showExp' n e
showExp' n (LET (f : fs) e) =
  "let\n"
    ++ showFun (n + 3) f
    ++ concatMap (\f -> "\n" ++ showFun (n + 3) f) fs
    ++ "\n"
    ++ printSpaces n
    ++ "in  "
    ++ showExp' n e

showBoolExp :: (Printer a, Printer b) => Int -> BoolExp a b -> String
showBoolExp n (Lt  e1 e2) = showExp' n e1 ++ "<" ++ showExp' n e2
showBoolExp n (Gt  e1 e2) = showExp' n e1 ++ ">" ++ showExp' n e2
showBoolExp n (Eq  e1 e2) = showExp' n e1 ++ "==" ++ showExp' n e2
showBoolExp n (AND e1 e2) = showBoolExp n e1 ++ "&&" ++ showBoolExp n e2
showBoolExp n (OR  e1 e2) = showBoolExp n e1 ++ "||" ++ showBoolExp n e2
showBoolExp n (NOT e    ) = "not(" ++ showBoolExp n e ++ ")"
