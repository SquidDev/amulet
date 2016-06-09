module Types.BuiltIn
  (
    typeNum, typeBool, typeStr, typeUnit, typeList
  )
where

import Syntax.Tree

typeNum :: Type
typeNum = TIdent $ QualifiedName ["Amulet"] "Num"

typeBool :: Type
typeBool = TIdent $ QualifiedName ["Amulet"] "Bool"

typeStr :: Type
typeStr = TIdent $ QualifiedName ["Amulet"] "Str"

typeUnit :: Type
typeUnit = TIdent $ QualifiedName ["Amulet"] "Unit"

typeList :: Type
typeList = TIdent $ QualifiedName ["Amulet"] "List"
