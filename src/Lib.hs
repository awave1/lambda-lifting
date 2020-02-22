module Lib
  ( someFunc
  )
where

import           AST

test1 = showProgram
  (Prog
    [ Fun ("main", [], ADD (VAR "X") (VAR "Y"))                 -- main() = X+Y
    , Fun                                                       -- fun f(x)
      ( "f"
      , ["x"]
      , LET                                                     -- let definition
        [ Fun ("g", ["y"], MUL (VAR "y") (VAR "x"))             -- g(y) = y*x
        , Fun ("h", ["x", "y"], DIV (VAR "x") (VAR "y"))        -- h(x, y) = x/y
        ]
        (ADD (APP "g" [VAR "x"]) (APP "h" [VAR "x", CONST 7])) -- in part: g(x) + h(x, 7)
      )
    ] :: (Prog String String)
  )

test2 = showProgram
  (Prog
    [ Fun ("main", [], ADD (VAR 1) (VAR 2))
    , Fun
      ( "f"
      , [1]
      , LET
        [ Fun ("g", [2], MUL (VAR 2) (VAR 2))
        , Fun ("h", [1, 2], DIV (VAR 1) (VAR 2))
        ]
        (ADD (APP "g" [VAR 1]) (APP "h" [VAR 1, CONST 7]))
      )
    ] :: (Prog String Int)
  )



someFunc :: IO ()
someFunc = putStrLn (test1 ++ "\n" ++ test2)
