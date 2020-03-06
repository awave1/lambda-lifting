module Lib
  ( lambdaLift
  )
where

import           AST
import           System.Environment
import           ParseProg                      ( parseFile
                                                , progToAST
                                                )
import           LambdaLifting.AlphaRenaming
import           LambdaLifting.CallGraph
import           LambdaLifting.Lifting


lambdaLift :: IO ()
lambdaLift = do
  args <- getArgs
  let file = head args
  programStr <- readFile file
  let ast = progToAST programStr
  putStrLn "Before: "
  putStrLn $ show_prog ast
  putStrLn "AST:"
  print ast
  putStrLn "------------------------------------------"

  putStrLn "After:"
  let renamedAst = alphaRename ast
  putStrLn $ show_prog renamedAst
  putStrLn "AST:"
  print renamedAst

  putStrLn "------------------------------------------"
  let callGraph = buildCallGraph renamedAst
  print callGraph
