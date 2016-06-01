{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PrettyPrint where

import Data.Monoid
import Syntax.Tree

import Data.List

data PrettyPrinter
  = PrettyPrinter { render :: String }
  deriving (Eq)

instance Show PrettyPrinter where
  show = render

instance Monoid PrettyPrinter where
  mempty = PrettyPrinter mempty
  mappend (PrettyPrinter x) (PrettyPrinter y) = PrettyPrinter $ x <> y

class Pretty a where
  pprint :: a -> PrettyPrinter
  pshow :: a -> String
  pshow = render . pprint

instance Pretty PrettyPrinter where
  pprint = id

instance Pretty String where
  pprint = PrettyPrinter

instance Pretty Name where
  pprint (ScopeName x) = pprint x
  pprint (QualifiedName xs x) = pprint $ intercalate "." xs ++ x

instance Pretty AccessLevel where
  pprint Public   = keyword "public"
  pprint Private  = keyword "private"
  pprint Internal = keyword "internal"

instance Pretty Type where
  pprint (TVar tv)  = typevar $ '\'':tv
  pprint (TIdent n) = type' n
  pprint (TInst t t') = type' t <+> "<" <+> type' t' <+> ">"
  pprint (TFunc t t') = type' t <+> " -> " <+> type' t'
  pprint (TTuple ts)  = parens $ intercalate ", " (map (pshow . type') ts)
  pprint (TForAll tv tc td) = keyword "forall " <+> '\'':tv <+> ". " <+> constraints tc <+> " => " <+> td
    where constraints [tc] = pshow tc
          constraints e@(x:xs) = "(" ++ intercalate ", " (map pshow e) ++ ")"


instance Pretty Literal where
  pprint (LString ls) = literal ls
  pprint (LNumber d) = literal $ show d
  pprint (LBoolean True) = literal "true"
  pprint (LBoolean False) = literal "false"
  pprint LUnit = literal "()"

instance Pretty Declaration where
  pprint (DName i) = pprint i
  pprint DDiscard = pprint "_"
  pprint (DTuple ds) = parens $ intercalate ", " $ map pshow ds

instance Pretty Assignable where
  pprint (AName i) = pprint i
  pprint (ATuple as) = parens $ intercalate ", " $ map pshow as

instance Pretty LetBinding where
  pprint (LetBinding a e True) = "mut " <+> a <+> " = " <+> e
  pprint (LetBinding a e False) = a <+> " = " <+> e

instance Pretty Expr where
  pprint (ELiteral l) = pprint l
  pprint (ETuple es) = parens $ intercalate ", " $ map pshow es
  pprint (EVar n) = pprint n
  pprint (EIndex e n) = e <+> "." <+> n
  pprint (EIf c t e) = keyword "if " <+> c <+> keyword " then " <+> t <+> keyword " else " <+> e
  pprint (EApply e v) = e <+> " " <+> v
  pprint (EBinOp o l r) = l <+> " " <+> o <+> " " <+> r
  pprint (ELambda n (Just t) b) = "\\" <+> parens (n <+> ": " <+> t) <+> " -> " <+> b
  pprint (ELambda e Nothing b) = "\\" <+> e <+> " -> " <+> b
  pprint (EUpcast e t) = e <+> " :> " <+> t
  pprint (EDowncast e t) = e <+> " ?> " <+> t
  pprint (ELet True vs e) = keyword "let rec " <+> intercalate " \x1b[1;32mand\x1b[0m " (map pshow vs) <+> keyword " in " <+> e
  pprint (ELet False vs e) = keyword "let " <+> intercalate " \x1b[1;32mand\x1b[0m " (map pshow vs) <+> keyword " in " <+> e
  pprint (EList es) = brackets $ intercalate "; " $ map pshow es
  pprint (EAssign a e c) = a <+> " <- " <+> e <+> " in " <+> c
  pprint (EMatch ps e) = keyword "match " <+> e <+> keyword " with\n" <+> pparms ps
    where pparm (pat, exp) = "    | " <+> pat <+> " -> " <+> exp
          pparms x = intercalate "\n" $ map (pshow . pparm) x

instance Pretty Pattern where
  pprint (PCapture i) = pprint i
  pprint PWildcard = pprint "_"
  pprint (PLiteral l) = pprint l
  pprint (POr ps) = pprint $ intercalate " || " $ map pshow ps
  pprint (PAnd ps) = pprint $ intercalate " || " $ map pshow ps
  pprint (PTuple ps) = parens $ intercalate ", " $ map pshow ps
  pprint (PList ps) = brackets $ intercalate "; " $ map pshow ps
  pprint (PPattern n p) = n <+> "@" <+> parens p

(<+>) :: (Pretty a, Pretty b) => a -> b -> PrettyPrinter
a <+> b = pprint a `mappend` pprint b
infixl 3 <+>

brackets :: Pretty a => a -> PrettyPrinter
brackets a = "[" <+> a <+> "]"

parens :: Pretty a => a -> PrettyPrinter
parens a = "(" <+> a <+> ")"

showpp :: Show a => a -> PrettyPrinter
showpp = pprint . show

colour :: Pretty a => a -> Integer -> PrettyPrinter
colour a n = clr <+> a <+> "\x1b[0m"
  where clr = "\x1b[1;3" ++ show n ++ "m"

keyword a = colour a 3
literal a = colour a 4
type' a = colour a 5
typevar a = colour a 6
