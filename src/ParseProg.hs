module ParseProg where

import           Parser.LexAssignment
import           Parser.ParAssignment
import           Parser.AbsAssignment
import           Parser.LayoutAssignment
import qualified Parser.AST                    as A
import           Parser.ASTConverter
import           Parser.ErrM

myLLexer = resolveLayout True . myLexer


progToAST :: String -> A.Prog String String
progToAST fileConts = case pTree of
  Bad emsg   -> error $ "Error in parsing \n" ++ emsg
  Ok  fpTree -> transProg fpTree
 where
  tokens = myLLexer fileConts
  pTree  = pProg tokens

parseFile :: String -> IO (A.Prog String String)
parseFile fname = do
  fconts <- readFile fname
  let ast = progToAST fconts
  return ast

showProg :: (A.Printer a, A.Printer b) => A.Prog a b -> String
showProg = A.show_prog
