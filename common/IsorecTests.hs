{- Collection of tests of isorec
 -}
module IsorecTests where

import Syntax
import TaplError

natListDecl = "NatList = Rec X. <nil:Unit, cons:{hd:Nat, tl:X}>; "
nlBodyDecl = "NLBody = <nil:Unit, cons:{hd: Nat, tl:NatList}>; "
nilDecl = "nil = fold [NatList] (<nil=unit> as NLBody);"
natListRes = "NatList :: *\n"
nilRes = natListRes ++ "NLBody :: *\n" ++ "nil : NatList\n"
         

-- FORMAT: (test name, expected printed output, input)
evalTests = [("declare Rec type", natListRes, natListDecl)
            ,("fold Rec type", nilRes,
              natListDecl ++ nlBodyDecl ++ nilDecl)
            ,("unfold folded Rec type", 
              nilRes ++ "<nil=unit> as NLBody : <nil:Unit, cons:{Nat,NatList}>",
              natListDecl ++ nlBodyDecl ++ nilDecl ++ "unfold [NatList] nil;")
            ]
