module Multiline where


{- |

>>> :{
    let
     x = 1
     y = z
   in x + y
  :}
  3

-}
z = 2

{- |

Aligns with the closing

>>> :{
    let
     x = 1
     y = z
   in x + y
     :}
     3
-}
z2 = 2


{- | Also works let that's for do:

>>> :{
let
     x = 1
     y = z
:}

>>> y
2

-}
z3 = 2



{- | Handles repeated @>>>@ too, which is bad since haddock-2.13.2 currently 
will strip the leading whitespace leading to something that will not copy-paste
(unless it uses explicit { ; } and the users manually strip the @>>>@)

>>> :{
>>> let
>>>      x = 1
>>>      y = z
>>> in x + y
>>> :}
3

-}
z4 = 4
