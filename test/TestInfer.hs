module TestInfer (inferTests) where

import Builders
import Syntax.Parser
import Syntax.Tree
import Infer
import Infer.Expr
import Infer.Run
import Test

checkInfer :: (String, Expr, Maybe Type) -> Test
checkInfer (name, expr, ty) = Test name $ return result
  where result = case (ty, inferExpr nullEnv expr) of
          (Just exp, Left got) -> mismatch exp got
          (Nothing,  Right  got) -> mismatch "<nothing>" got

          (Nothing,  Left _)   -> Pass
          (Just exp, Right got)  -> if exp == got then Pass else mismatch exp got


types :: [(String, Expr, Maybe Type)]
types = map build [
  ("2",       Just typeNum),
  ("\"x\"",   Just typeStr),
  ("true",    Just typeBool),
  ("()",      Just typeUnit),
  ("(2, \"x\")", Just $ TTuple [ typeNum, typeStr ]),
  ("\\x -> x", Just $ forAll "a" $ TFunc (TVar "a") (TVar "a")),
  ("let f = \\x -> x in f 2", Just  typeNum),
  ("\\x y -> (x, y)", Just $ forAll "b" $ forAll "a" $ TFunc (TVar "a") $ TFunc (TVar "b") $ TTuple [ TVar "a", TVar "b" ]),
  ("\\(x : ['a]) -> x", Just $ forAll "a" $ TFunc (TInst typeList $ TVar "a") (TInst typeList $ TVar "a")),
  ("\\(x: ['a] -> 'a) -> x [1; 2; 3]", Just $ TFunc (TFunc (TInst typeList typeNum) typeNum) typeNum),
  ("let x = x in x", Nothing),
  ("let rec x = x in x", Just $ forAll "a" $ TVar "a"),
  ("let (a, b) = (true, 1) in (b, a)", Just $ TTuple [ typeNum, typeBool ]),
  ("let rec f = \\x y -> if x then [y] else f false y in f", Just $ forAll "c" $ TFunc typeBool $ TFunc (TVar "c") $ TInst typeList $ TVar "c"),
  ("let rec f = \\x y -> if x then [y] else f false 1 in f", Just $ TFunc typeBool $ TFunc typeNum $ TInst typeList typeNum),
  ("\\x -> x x", Nothing),
  ("let rec f = \\x -> f in f", Nothing),
  ("match [1; 2; 3] with | [a; b] -> (a, b)", Just $ TTuple [ typeNum, typeNum ]),
  ("match (1, 2) with | (a, _) -> a", Just typeNum),
  ("match (1, 2) with | (false, a) -> a", Nothing),
  ("match (1, true) with | x@(a, b) -> (x, a, b)", Just $ TTuple [ TTuple [ typeNum, typeBool ], typeNum, typeBool ])
  ] where build (expr, ty) = (expr, extract expr $ parseExpr expr, ty)
          extract expr (Left err) = error $ (expr ++ " => " ++ show err)
          extract _ (Right x) = x

inferTests :: Test
inferTests = Group "Infer" $ map checkInfer types
