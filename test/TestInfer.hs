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
  ("\\x y -> (x, y)", Just $ forAll "b" $ forAll "a" $ TFunc (TVar "a") $ TFunc (TVar "b") $ TTuple [ TVar "a", TVar "b" ])
  ] where build (expr, ty) = (expr, extract $ parseExpr expr, ty)
          extract (Left _) = undefined
          extract (Right x) = x

inferTests :: Test
inferTests = Group "Infer" $ map (checkInfer) types
