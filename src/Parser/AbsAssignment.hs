

module Parser.AbsAssignment where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
data Prog = PROG [Fun]
  deriving (Eq, Ord, Show, Read)

data Fun = FUNS Ident [Ident] Exp
  deriving (Eq, Ord, Show, Read)

data Exp = EXP_ADD Exp Term | EXP_SUB Exp Term | EXP_Term Term
  deriving (Eq, Ord, Show, Read)

data Term
    = TERM_MUL Term Factor | TERM_DIV Term Factor | TERM_FACT Factor
  deriving (Eq, Ord, Show, Read)

data Factor
    = FACT_CONST Integer
    | FACT_VAR Ident
    | FACT_NEG Exp
    | FACT_APP Ident [Exp]
    | FACT_LET [Fun] Exp
    | EXP_COND BExp Exp Exp
  deriving (Eq, Ord, Show, Read)

data BExp
    = BEXP_AND BExp BTerm | BEXP_OR BExp BTerm | BEXP_TERM BTerm
  deriving (Eq, Ord, Show, Read)

data BTerm
    = BTERM_GT Exp Exp
    | BTERM_LT Exp Exp
    | BTERM_EQ Exp Exp
    | BTERM_NOT BExp
    | BTERM_TRUE
    | BTERM_FALSE
  deriving (Eq, Ord, Show, Read)

