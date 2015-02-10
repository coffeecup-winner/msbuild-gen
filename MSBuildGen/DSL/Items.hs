{-# LANGUAGE TemplateHaskell #-}
module MSBuildGen.DSL.Items ( items
                            , string
                            , list
                            , path
                            , bool
                            , customString
                            , customList
                            , customPath
                            , customBool
                            ) where

import Control.Monad
import Language.Haskell.TH

import MSBuildGen.DSL.Core
import MSBuildGen.Types

items :: [String] -> DecsQ
items ns = liftM concat $ mapM item ns

item :: String -> DecsQ
item n = simpleData n $ itemInstances n

itemInstances :: String -> DecsQ
itemInstances n = [d| instance Item $(itemType) where toMSBuildItem _ = Item $(itemNameExp) |]
    where itemType = conT $ mkName n
          itemNameExp = litE $ stringL n

string :: String -> DecsQ
string n = customString n n

list :: String -> DecsQ
list n = customList n n

path :: String -> DecsQ
path n = customPath n n

bool :: String -> DecsQ
bool n = customBool n n

customString :: String -> String -> DecsQ
customString = metadata 'String

customList :: String -> String -> DecsQ
customList = metadata 'List

customPath :: String -> String -> DecsQ
customPath = metadata 'Path

customBool :: String -> String -> DecsQ
customBool = metadata 'Bool

metadata :: Name -> String -> String -> DecsQ
metadata t n v = simpleData n $ metadataInstances t n v

metadataInstances :: Name -> String -> String -> DecsQ
metadataInstances t n v = [d| instance ItemMetadata $(metadataType) where toMSBuildItemMetadata _ = ItemMetadata $(typeExp) $(metadataNameExp)
                              instance Condition $(metadataType) where toMSBuildCondition = toMSBuildCondition . toMSBuildItemMetadata
                          |]
    where metadataType = conT $ mkName n
          metadataNameExp = litE $ stringL v
          typeExp = conE t
