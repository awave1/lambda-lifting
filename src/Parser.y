{
{-# OPTIONS -w #-}
module Parser( parseExp ) where

import Language
import Lexer

}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
-- Without this we get a type error
%error { happyError }

%token 
  fun             { Token _ TokenFun }
  where           { Token _ TokenWhere }
  let             { Token _ TokenLet }
  in              { Token _ TokenIn }
  int             { Token _ (TokenInt $$) }
  var             { Token _ (TokenVar $$) }
  '='             { Token _ TokenEq }
  '+'             { Token _ TokenPlus }
  '-'             { Token _ TokenMinus }
  '*'             { Token _ TokenTimes }
  '/'             { Token _ TokenDiv }
  '('             { Token _ TokenLParen }
  ')'             { Token _ TokenRParen }
  ';'             { Token _ TokenSemi }
  ','             { Token _ TokenComma }

%%

Exp   : fun var Params '=' let var '=' Exp in Exp { Fun $2 $3 (Let $6 $8 $10) }
      | fun var Params '=' Exp where var '=' Exp  { Fun $2 $3 (Where $5 $7 $9) }
      | fun var Params '=' Exp                    { Fun $3 $3 $5 }
      | Exp1                                      { Exp1 $1 }

Params: '(' ')'                       { [] }
      | '(' Params_ ')'               { $2 }

Params_: Param             { [$1] }
       | Params_ ',' Param { $3:$1 }

Param: var { Var $1 }

Exp1  : Exp1 '+' Term           { Plus $1 $3 }
      | Exp1 '-' Term           { Minus $1 $3 }
      | Term                    { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor        
      : int                     { Int $1 }
      | var                     { Var $1 }
      | '(' Exp ')'             { Brack $2 }

{
lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
  alexError' p ("parse error at token '" ++ unLex t ++ "'")

parseExp :: FilePath -> String -> Either String Exp
parseExp = runAlex' parse
}
