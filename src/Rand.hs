module Rand where

import           System.Random
import           Control.Monad.Trans.State

rollDie :: State StdGen Int
rollDie = do
  generator <- get
  let (val, newGenerator) = randomR (1, 6) generator
  put newGenerator
  return val
