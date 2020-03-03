module AlphaRename where

import           Data.List
import           Data.Maybe
import           Data.Map                       ( Map )
import           Control.Monad.Trans.State
import qualified Data.Map                      as Map
import qualified AST                           as A


alphaRename :: A.Prog String String -> A.Prog String String
alphaRename (A.Prog funs) = undefined

-- replaceFunName :: A.Fun String String -> A.Fun String String

-- -- 
-- replaceArgs :: [String] -> Int -> [String]
-- replaceArgs [] _ = []
-- replaceArgs (_ : args) index =
--   replaceArgs (("x" ++ show index) : args) (index + 1)

-- get the list of all functions in the Prog
getFuns :: A.Prog String String -> [A.Fun String String]
getFuns (A.Prog funs) = funs
