1. a renaming

   1. state monad

   ```hs
   Fun (f, _, expr)
   -- replace f with f_n where n is the variable(?)
   -- then, fun.num++;
   -- in expr, substitute all occurrences of f with f_n
   ```

   ```hs
   -- have sub function
   sub :: a -> Fun a b -> Fun a b
   sub new Fun (f, args, expr) = Fun (new, args, expr')
    where expr' = has all occurences of f replaced with new

    -- something similar for variablies
    -- + should incorporate state
   ```

2. call graph
   1. use map -> haskell for dict: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map.html
   ```hs
   map a       [a]
       func     every function symbol called
       symbol

   map a [a] -> Fun a b -> map a [b]
   -- look at every function symbol (the topmost, e.g. ignore let and the function body?)
   -- adjacency list for the graph
   ```
3. lambda lifting

   1. Vargs & Vfree
   2. keep doing for every link in the graph (use Data.Set (?))

   ```hs
   map a -> (Set b, Set b)
               fun, free
   ```

   3. step through, choose an order how to go through
   4. use dijkstra's to find the spanning tree(?)
   5. can just do the exact formula using set datatype
   6. keep going

---

```
(AST.Fun ("f",["v1"],AST.LET [AST.Fun ("g",["v2"],AST.MUL (AST.VAR "v2") (AST.VAR "v2"))] (AST.ADD (AST.APP "g" [AST.VAR "v1"]) (AST.VAR "v1"))))
```

```
(AST.Fun ("main",["x","y","z"],AST.LET [AST.Fun ("f",["y"],AST.ADD (AST.VAR "x") (AST.APP "g" [AST.VAR "y"])),AST.Fun ("g",["z"],AST.LET [AST.Fun ("f",["x"],AST.MUL (AST.VAR "x") (AST.VAR "z"))] (AST.APP "f" [AST.VAR "x"]))] (AST.ADD (AST.APP "g" [AST.VAR "z"]) (AST.APP "f" [AST.VAR "x"]))))
```

```
(AST.Fun ("main",["x","y"],AST.LET [AST.Fun ("f",["y"]
,AST.ADD (AST.VAR "x") (AST.VAR "y"))] (AST.ADD (AST.APP "f" [AST.VAR "x"]) (AST.VAR "y"))))
```

```hs
AST.Prog [AST.Fun ("f",["v1"],AST.LET [AST.Fun ("g",["v2"],AST.MUL (AST.VAR "v2") (AST.VAR "v2")),AST.Fun ("h",["v3"],AST.ADD (AST.APP "g" [AST.VAR "v3"]) (AST.VAR "v3"))] (AST.ADD (AST.APP "g" [AST.VAR "v1"]) (AST.VAR "v1")))]
```
