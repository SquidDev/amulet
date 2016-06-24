module Codegen.Emit where

import Codegen.Lua
import Data.List

emitS :: Statement -> String
emitS (Do xs) = concat ["do ", intercalate "; " $ map emitS xs, " end"]
emitS (Set n (Function ars dt b)) = concat [ "local function ", emitN n, "(", intercalate ", " ars, ") "
                                           , intercalate "; " $ map emitS b, " end" ]
emitS (Set n e) = concat ["local ", emitN n, " = ", emitE e]
emitS (While c b) = concat ["while ", emitE c, emitS (Do b)]
emitS (Repeat c b) = concat ["repeat ", intercalate "; " $ map emitS b, " until ", emitE c]
emitS (If (x:xs) els) = concat [ mkif x, intercalate " " $ map mkeif xs
                               , mkels els
                               , " end"]
    where mkif (x, b)  = concat ["if ", emitE x, " then ", intercalate "; " $ map emitS b]
          mkeif (x, b) = concat [" elseif ", emitE x, " then ", intercalate "; " $ map emitS b]
          mkels [] = ""
          mkels b = concat [" else ", intercalate "; " $ map emitS b]

emitS (Local nms) = concat ["local ", intercalate ", " $ map fst nms, " = ", intercalate ", " $ map (emitE . snd) nms]
emitS (SApply ap) = emitA ap
emitS (Return e) = concat ["return ", emitE e]

emitE :: Expr -> String
emitE Nil = "nil"
emitE Dots = "..."
emitE Codegen.Lua.True = "true"
emitE Codegen.Lua.False = "false"
emitE (Table prs) = concat ["{ ", intercalate ", " $ map emitP prs, " }"]
  where emitP (i, e) = concat ["[", emitE i, "] = ", emitE e]
emitE (EApply ap) = emitA ap
emitE (Tuple e) = intercalate ", " . map emitE $ e
emitE (Index e x) = concat [emitE e, "[", x, "]"]
emitE (Name n) = emitN n
emitE (Number d) = show d
emitE (String s) = show s
emitE (Function ars dt b) = concat [ "function(", intercalate ", " ars, (if dt then "..." else ""), ") "
                                   , intercalate "; " $ map emitS b, " end" ]


emitE (Op l o r) = make r
  where make (Just x) = concat [emitE l, " ", emitO o, " ", emitE x]
        make Nothing = concat [emitO o, emitE l]

emitN :: Name -> String
emitN (Scoped x) = x
emitN (Qualified xs x) = intercalate ", " $ xs ++ [x]

emitA :: Application -> String
emitA (Call e a) = concat ["(", emitE e, ")", "(", intercalate ", " $ map emitE a, ")"]
emitA (Invoke t m a) = concat [t, ":", m, "(", intercalate ", " $ map emitE a, ")"]

emitO :: Operator -> String
emitO Add = "+"
emitO Sub = "-"
emitO Mul = "*"
emitO Div = "/"
emitO Mod = "%"
emitO Pow = "^"
emitO Concat = ".."
emitO Eq = "=="
emitO Lt = "<"
emitO Le = "<="
emitO And = " and "
emitO Or = " or "
emitO Not = " not "
emitO Len = "#"
emitO Shr = ">>"
emitO Shl = "<<"
