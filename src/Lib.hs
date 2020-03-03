module Lib
  ( lambdaLift
  )
where

import           AST
import           System.Environment
import           ParseProg                      ( parseFile
                                                , progToAST
                                                )
import           AlphaRename


lambdaLift :: IO ()
lambdaLift = do
  args <- getArgs
  let file = head args
  programStr <- readFile file
  let ast = progToAST programStr
  putStrLn "Before: "
  putStrLn $ show_prog ast
  putStrLn "After:"
  putStrLn $ show_prog $ alphaRename ast
