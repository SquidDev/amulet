{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Pretty where

import Control.Monad.Reader
import Control.Monad.Writer

import Syntax.Tree
import Data.List
import Infer

type PrettyPrinter = ReaderT PParam
                      (Writer String ) ()

data PParam
  = PParam { colours       :: Bool
           , typeColour    :: Integer
           , keywordColour :: Integer
           , typevarColour :: Integer
           , literalColour :: Integer }
    deriving (Eq, Show)


runPrinter :: PrettyPrinter -> PParam -> String
runPrinter act ctx = let (_, ret) = runWriter (runReaderT act ctx) in ret

colour :: Pretty a => Integer -> a -> PrettyPrinter
colour clr cmb = do
  x <- asks colours
  tell $ if x then
    "\x1b[1;3" <> show clr <> "m"
  else
    ""
  y <- pprint cmb
  tell "\x1b[0m"
  return y

type' :: Pretty a => a -> PrettyPrinter
type' x = do
  clr <- asks typeColour
  colour clr x

keyword :: Pretty a => a -> PrettyPrinter
keyword x = do
  clr <- asks keywordColour
  colour clr x

typevar :: Pretty a => a -> PrettyPrinter
typevar x = do
  clr <- asks typevarColour
  colour clr x

literal :: Pretty a => a -> PrettyPrinter
literal x = do
  clr <- asks literalColour
  colour clr x

delim :: Pretty a => (String, String) -> a -> PrettyPrinter
delim (s, e) y = do
  tell s
  ret <- pprint y
  tell e
  return ret

between :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> PrettyPrinter
between a b c = pprint a >> pprint c >> pprint b

parens, braces, squares, angles :: Pretty a => a -> PrettyPrinter
parens  = delim ("(", ")")
braces  = delim ("{", "}")
squares = delim ("[", "]")
angles  = delim ("<", ">")

defaults :: PParam
defaults = PParam { colours = True
                  , keywordColour = 3
                  , literalColour = 4
                  , typeColour    = 5
                  , typevarColour = 6 }

colourless :: PParam
colourless = defaults { colours = False }

class Pretty a where
  pprint :: a -> PrettyPrinter

instance Pretty String where pprint = tell

instance Pretty PrettyPrinter where pprint = id

instance Pretty Name where
  pprint (ScopeName x) = pprint x
  pprint (QualifiedName xs x) = intercalate "." xs <+> "." <+> x

instance Pretty AccessLevel where
  pprint Public   = keyword "public"
  pprint Private  = keyword "private"
  pprint Internal = keyword "internal"

instance Pretty Type where
  pprint (TVar tv) = typevar $ '\'':tv
  pprint (TIdent n) = type' n
  pprint (TInst t t') = type' t <+> angles (typevar t')
  pprint (TFunc t@TFunc{} t') = do
    parens $ pprint t
    tell " -> "
    pprint t'

  pprint (TForAll tv tc td) = do
    between (keyword "forall ") ". " $ typevar $ '\'':tv
    constraints tc
    pprint td
      where constraints [] = pprint ""
            constraints [tc] = tc <+> " => "
            constraints e@(_:_) = do
              x <- ask
              let psh y = runPrinter (pprint y) x in
                  "(" ++ intercalate ", " (map psh e) ++ ") " <+> " => "


  pprint (TFunc t t') = t <+> " -> " <+> t'
  pprint (TTuple ts) = do
    x <- ask
    parens $ intercalate " * " $ map (`ppshow` x) ts

instance Pretty Literal where
  pprint (LString ls) = literal $ "\"" ++ ls ++ "\""
  pprint (LNumber d) = literal $ show d
  pprint (LBoolean True) = literal "true"
  pprint (LBoolean False) = literal "false"
  pprint LUnit = literal "()"

instance Pretty (Name, Declaration) where
  pprint (x, y) = x <+> " = " <+> y

instance Pretty Declaration where
  pprint (DName i) = pprint i
  pprint DDiscard = literal "_"
  pprint (DTuple ds) = parens $ do
    x <- ask
    pprint $ intercalate ", " $ map (`ppshow` x) ds
  pprint (DRecord ts) = braces $ do
    x <- ask
    pprint $ intercalate "; " $ map (`ppshow` x) ts

instance Pretty Assignable where
  pprint (AName i) = pprint i
  pprint (ATuple as) = do
    x <- ask
    parens $ intercalate ", " $ map (`ppshow` x) as

instance Pretty LetBinding where
  pprint (LetBinding a e True) = keyword "mut " <+> a <+> " = " <+> e
  pprint (LetBinding a e False) = a <+> " = " <+> e

instance Pretty Expr where
  pprint (ELiteral l) = pprint l
  pprint (ETuple es) = parens $ do
    x <- ask
    pprint $ intercalate ", " $ map (`ppshow` x) es
  pprint (EVar n) = pprint n
  pprint (EIndex e n) = e <+> "." <+> n
  pprint (EIf c t e) = do
    keyword "if"
    pprint c
    keyword " then "
    pprint t
    keyword " else "
    pprint e

  pprint (EApply e v) = e <+> " " <+> v
  pprint (EBinOp o l r) = l <+> " " <+> o <+> " " <+> r
  pprint (ELambda n (Just t) b) = "\\" <+> parens (n <+> ": " <+> t) <+> " -> " <+> b
  pprint (ELambda e Nothing b) = "\\" <+> e <+> " -> " <+> b
  pprint (EUpcast e t) = e <+> " :> " <+> t
  pprint (EDowncast e t) = e <+> " ?> " <+> t
  pprint (ELet True vs e) = do x <- ask
                               pprint "let rec "
                               pprint $ intercalate (keyword " and " `ppshow` x) $ map (`ppshow` x) vs
                               keyword " in "
                               pprint e
  pprint (ELet False vs e) = do x <- ask
                                pprint "let "
                                pprint $ intercalate (keyword " and " `ppshow` x) $ map (`ppshow` x) vs
                                keyword " in "
                                pprint e
  pprint (EList es) = ask >>= \x -> squares $ intercalate "; " $ map (`ppshow` x) es
  pprint (EAssign a e c) = a <+> " <- " <+> e <+> " in " <+> c
  pprint (EMatch ps e) = keyword "match " <+> e <+> keyword " with\n" <+> pparms ps
    where pparms :: [(Pattern, Expr)] -> PrettyPrinter
          pparm :: (Pattern, Expr) -> PrettyPrinter
          pparm (pat, exp) = "    | " <+> pat <+> " -> " <+> exp
          pparms x = do
            env <- ask
            let psharm y = pparm y `ppshow` env in
                pprint $ intercalate "\n" $ map psharm x

instance Pretty Pattern where
  pprint (PCapture i) = pprint i
  pprint PWildcard = pprint "_"
  pprint (PLiteral l) = pprint l
  pprint (POr ps) = ask >>= \x -> pprint $ intercalate " | " $ map (`ppshow` x) ps
  pprint (PAnd ps) = ask >>= \x -> pprint $ intercalate " & " $ map (`ppshow` x) ps
  pprint (PTuple ps) = ask >>= \x -> parens $ intercalate ", " $ map (`ppshow` x) ps
  pprint (PList ps) = ask >>= \x -> squares $ intercalate "; " $ map (`ppshow` x) ps
  pprint (PPattern n p) = n <+> "@" <+> parens p

instance Pretty TypeError where
  pprint (UnificationFail t1 t2) = "Cannot unify " <+> t1 <+> " and " <+> t2
  pprint (InfiniteType var ty) = "Infinite type for type variable " <+> typevar (pprint $ '\'':var) <+> " under type " <+> ty
  pprint (UnboundVariable var) = "Unbound variable " <+> pprint var
  pprint (Ambigious cons) = do
    env <- ask
    pprint "Ambiguous between "
    parens $ intercalate ", " $ map ((`ppshow` env) . pcons) cons
      where pcons (l, r, e) = l <+> " <:> " <+> r <+> " in " <+> e

  pprint (UnificationMismatch t1 t2) = "Varying lengths for " <+> m t1 <+> " and " <+> m t2
    where m ts = do env <- ask
                    parens $ intercalate ", " $ map (`ppshow` env) ts
  pprint (TypeContext t1 t2 err) = err <+> "\n  when unifying " <+> t1 <+> " and " <+> t2
  pprint (ExprContext expr err)  = err <+> "\n in " <+> expr

ppshow :: Pretty a => a -> PParam -> String
ppshow = runPrinter . pprint

(<+>) :: (Pretty a, Pretty b) => a -> b -> PrettyPrinter
a <+> b = pprint a >> pprint b
infixl 3 <+>

pshow :: Pretty a => a -> String
pshow = (`runPrinter` defaults) . pprint
