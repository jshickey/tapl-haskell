{- Collection of tests of isorec
 -}
module IsorecTests where

import Syntax
import TaplError

natListDecl = "NatList = Rec X. <nil:Unit, cons:{Nat,X}>; "
nlBodyDecl = "NLBody = <nil:Unit, cons:{Nat,NatList}>; "
nilDecl = "nil = fold [NatList] (<nil=unit> as NLBody);"

-- FORMAT: (test name, expected printed output, input)
evalTests = [("declare Rec type", "NatList :: *", natListDecl)
            ,("fold Rec type", "nil : NatList",
              natListDecl ++ nlBodyDecl ++ nilDecl)
            ,("unfold folded Rec type",
              "<nil=unit> as NLBody : <nil:Unit, cons:{Nat,NatList}>",
              natListDecl ++ nlBodyDecl ++ nilDecl ++ "unfold [NatList] nil")
            ]
