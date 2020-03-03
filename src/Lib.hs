module Lib
  ( lambdaLift
  )
where

import           AST
import           System.Environment
import           ParseProg                      ( parseFile )
import           AlphaRename


lambdaLift :: IO ()
lambdaLift = do
  args <- getArgs
  let file = head args
  parsedResult <- parseFile file
  print parsedResult
