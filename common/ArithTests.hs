{- Because arith is supported by more than one implementation (e.g.,
   both arith and fulluntyped, we extract eval tests into a common module
 -}

module ArithTests where

-- Each element in list contains: (label, expected, input)
arithEvalTests
    = [("true", "true", "true;"),
       ("false", "false", "false;"),
       ("zero", "0", "0;"),
       ("E-IfTrue", "0", "if true then 0 else false;"),
       ("E-IfFalse", "false", "if false then 0 else false;"),
       ("E-If", "0", "if if false then false else true then 0 else false;"),
       ("succ", "1", "succ 0;"),
       ("E-Succ", "1", "succ if true then 0 else false;"),
       ("E-PredSucc", "0", "pred (succ 0);"),
       ("E-PredZero", "0", "pred 0;"),
       ("E-Pred", "0", "pred (pred 0);"),
       ("test.f #4", "1", "succ (pred 0);"),
       ("test.f #5", "false", "iszero (pred (succ (succ 0)));"),
       ("E-IsZeroZero", "true", "iszero 0;"),
       ("E-IsZeroSucc", "false", "iszero (succ 0);"),
       ("E-IsZero", "true", "iszero (pred (succ 0));"),
       -- test the full sample file from the original arith impl
       ("full arith test.f", "true\nfalse\n0\n1\nfalse", "/* Examples for testing */\n\ntrue;\nif false then true else false; \n\n0; \nsucc (pred 0);\niszero (pred (succ (succ 0))); \n")
      ];
