module Lib
  ( lambdaLift
  )
where

import           AST
import           System.Environment
import           Parser                         ( parseExp )

lambdaLift :: IO ()
lambdaLift = do
  args   <- getArgs
  result <- case args of
    []  -> fmap (parseExp "<stdin>") getContents
    [f] -> fmap (parseExp f) (readFile f)
    _   -> error "expected 1 arg"
  either putStrLn print result
