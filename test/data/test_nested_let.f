fun foo (a,b,c) = let
  fun bar (x,y,z) = let 
      fun baz (p,q,r) = p * q *r
    in baz(a,x,y) + baz (b,y,z)   
  
  fun smh (y,z) = y - z
in bar (a,b,c) + smh (b,c)