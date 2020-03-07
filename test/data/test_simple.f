fun f(v1, v2) = let
  fun g(v) = v * v1
  fun h(x) = g(x) + x
in g(v1) + h(v2)