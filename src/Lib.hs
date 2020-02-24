module Lib
  ( lambdaLift
  )
where

import           AST
import           System.Environment

lambdaLift :: IO ()
lambdaLift = do
  args <- getArgs
  file <- readFile $ head args
  putStrLn file
