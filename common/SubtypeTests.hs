{- Collection of subtype tests used by several implementations
 -}
module SubtypeTests where

import TaplError

-- for the bot implementation
botEvalTests =
    [("apply bot", "(lambda x:Bot. x x) : Bot -> Bot",
      "lambda x:Bot. x x;")
    ,("abs type match", "(lambda x:Top. x) : Top -> Top",
      "(lambda x:Top->Top. x) (lambda x:Top. x);")
    ,("abs subtype 1", "(lambda x:Top. x) : Bot -> Top",
      "(lambda x:Bot->Top. x) (lambda x:Top. x);")
    ,("abs subtype 2", "(lambda x:Bot. x) : Bot -> Top",
      "(lambda x:Bot->Top. x) (lambda x:Bot. x);")
    ]

botEvalErrorTests =
    [("apply top", show notAbstraction,
      "lambda x:Top. x x;")
    ,("bad abs subtype 1", show badApplication,
      "(lambda x:Top->Top. x) (lambda x:Bot. x);")
    ,("bad abs subtype 2", show badApplication,
      "(lambda x:Bot->Bot. x) (lambda x:Top. x);")
    ]

-- for the rcdsubbot implementation, which expands upon bot
rcdsubEvalTests = botEvalTests ++
    [("type match", "{x=(lambda m:Top. m)} : {x:Top -> Top}",
     "(lambda a:{x:Top->Top}. a) {x=(lambda m:Top. m)};")
    ,("app subtype", "{x=(lambda m:Top. m)} : {x:Bot -> Top}",
      "(lambda a:{x:Bot->Top}. a) {x=(lambda m:Top. m)};")
    ,("width subtype 1", "{x=(lambda m:Top. m), y=(lambda n:Bot. n)} : {x:Bot -> Top}",
      "(lambda a:{x:Bot->Top}. a) {x=(lambda m:Top. m),y=(lambda n:Bot. n)};")
    ,("width subtype 2", "{y=(lambda n:Bot. n), x=(lambda m:Top. m)} : {x:Bot -> Top}",
      "(lambda a:{x:Bot->Top}. a) {y=(lambda n:Bot. n), x=(lambda m:Top. m)};")
    ,("permutation subtype", 
      "(lambda m:Top. m) : Top -> Top",
      "(lambda a:{x:Top->Top, y:Bot->Bot}. a.x) {y=(lambda n:Bot. n), x=(lambda m:Top. m)};")
    ,("permutation + width subtype",
      "(lambda n:Bot. n) : Bot -> Bot",
      "(lambda a:{x:Bot->Top, y:Bot->Bot}. a.y) {y=(lambda n:Bot. n), x=(lambda m:Top. m)};")
    ]

rcdsubEvalErrorTests = botEvalErrorTests ++
    [("notsubtype 1", show badApplication, 
      "(lambda a:{x:Top->Top}. a) {x=(lambda m:Bot. m)};")
    ,("notsubtype 2", show badApplication, 
      "(lambda a:{x:Bot->Bot}. a) {x=(lambda m:Top. m)};")
    ,("bad width subtype", show badApplication, 
      "(lambda a:{x:Bot->Top}. a) {z=(lambda m:Top. m),y=(lambda n:Bot. n)};")
    ,("bad permutation subtype", show badApplication,
      "(lambda a:{x:Top->Top, z:Bot->Bot}. a.x) {y=(lambda n:Bot. n), x=(lambda m:Top. m)};")
    ]

-- for fullref and fullsub, which expand upon rcdsubbot
fullsubEvalTests
    = rcdsubEvalTests ++
      [("if join 1", "{y=false} : {}",
       "if true then {y=false} else {x=true};")
      ,("if join 2", "{x=false} : {x:Bool}",
       "if true then {x=false} else {y=true,x=true};")
      ,("if join 3: join in field", "{x=false} : {x:Top}",
       "if true then {x=false} else {y=true,x=0};")
      ,("if join 4: join lambdas in field", 
        "{x=false, z=(lambda a:Bot -> Top. a)} : {x:Top, z:(Top -> Top) -> Bot -> Top}",
       "if true then {x=false,z=(lambda a:Bot->Top. a)} else {z=(lambda b:Top->Top. b),y=true,x=0};")
      ,("ascribe 1", "(lambda x:Bot. x) : Bot -> Top",
       "(lambda x:Bot. x) as Bot->Top;")
      ,("ascribe 2", "(lambda x:Top. x) : Bot -> Top",
       "(lambda x:Top. x) as Bot->Top;")
      ,("fix 1", "(lambda x:Nat. x) : Nat -> Nat",
       "fix (lambda f:Nat->Top. (lambda x:Nat. x));")
      ,("fix 2", "(lambda x:Top. true) : Top -> Bool",
       "fix (lambda f:Nat->Bool. (lambda x:Top. true));")
      ]

fullsubEvalErrorTests
    = rcdsubEvalErrorTests ++
      [("bad ascribe 1", show ascribeError,
       "(lambda x:Bot. x) as Top->Bot;")
      ,("bad ascribe 2", show ascribeError,
       "(lambda x:Top. x) as Top->Bot;")
      ,("bad fix 1", show fixError,
       "fix (lambda f:Nat->Bot. (lambda x:Nat. x));")
      ,("bad fix 2", show fixError,
       "fix (lambda f:Top->Nat. (lambda x:Nat. x));")
      ,("bad fix 3", show fixError,
       "fix (lambda f:Nat->Bool. (lambda x:Bot. true));")
      ]