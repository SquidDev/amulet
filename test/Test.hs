module Test (tests) where

import Syntax.Tree
import Infer
import Infer.Run
import Distribution.TestSuite
import PrettyPrint

mismatch :: Pretty a => Pretty b => a -> b -> Result
mismatch a b = Fail $ "Expected " ++ pshow a ++ "\n     got " ++ pshow b

checkInfer :: (Expr, Maybe Type) -> TestInstance
checkInfer (expr, ty) = test
  where result = case (ty, inferExpr nullEnv expr) of
          (Just exp, Left got) -> mismatch exp got
          (Nothing,  Right  got) -> mismatch "<nothing>" got

          (Nothing,  Left _)   -> Pass
          (Just exp, Right got)  -> if exp == got then Pass else mismatch exp got
        test = TestInstance {
          run = return $ Finished $ result,
          name = "Infer " ++ show expr,
          tags = ["infer", "type"], options = [], setOption = \_ _ -> Right test
          }

evar :: Ident -> Expr
evar = EVar . ScopeName

forAll :: TypeVar -> Type -> Type
forAll name ty = TForAll { var = name, cons = [], td = ty }

types :: [(Expr, Maybe Type)]
types = [
  (ELambda "x" Nothing $ evar "x", Just $ forAll "a" $ TFunc (TVar "a") (TVar "a"))
        ]
tests :: IO [Test]
tests = return $ map (Test . checkInfer) types
