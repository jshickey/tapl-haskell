{- Collection of tests of equirec
 -}
module EquirecTests where

import Syntax
import TaplError

-- FORMAT: (test name, expected parse tree, input)
parseTests = [("comments", TmTrue, "TODO")
             ]
    
-- FORMAT: (test name, expected printed output, input)
evalTests = [("unfold fold", "TODO",
              "TODO")
            ]

evalErrorTests = [("todo", "todo",
                  "todo")
                 ]