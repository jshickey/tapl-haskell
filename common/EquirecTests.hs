{- Collection of tests of equirec
 -}
module EquirecTests where

import Syntax
import TaplError

-- FORMAT: (test name, expected printed output, input)
evalTests = [("define Rec type", "NatList :: *",
              "NatList = Rec X. <nil:Unit, cons:{Nat,X}>;")
            ,("instance of Rec type", "nil : NatList",
              "NatList = Rec X. <nil:Unit, cons:{Nat,X}>; nil = <nil=unit> as NatList;")
            ]

