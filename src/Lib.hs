module Lib
  ( someFunc
  )
where

import           AST

test1 = showProg
  (Prog
    [ Fun ("main", [], ADD (VAR "X") (VAR "Y"))
    , Fun
      ( "f"
      , ["x"]
      , LET
        [ Fun ("g", ["y"], MUL (VAR "y") (VAR "x"))
        , Fun ("h", ["x", "y"], DIV (VAR "x") (VAR "y"))
        ]
        (ADD (APP "g" [VAR "x"]) (APP "h" [VAR "x", CONST 7]))
      )
    ] :: (Prog String String)
  )


test2 = showProg
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
