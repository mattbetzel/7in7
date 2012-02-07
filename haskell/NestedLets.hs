bar = let x = 1
      in ((let x = "foo" in x), x)

quux a = let a = "foo"
         in a ++ "eek!"

myfoo = let a = "foo" in "foo2"
