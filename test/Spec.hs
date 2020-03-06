import           AST

test1 :: String
test1 = show_prog
  (Prog
    [ Fun                                                       -- fun f(x)
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

test2 :: String
test2 = show_prog
  (Prog
    [ Fun
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

main :: IO ()
main = putStrLn ("\n" ++ test1 ++ "\n" ++ test2)
