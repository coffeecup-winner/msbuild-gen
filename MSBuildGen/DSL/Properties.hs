{-# LANGUAGE TemplateHaskell #-}
module MSBuildGen.DSL.Properties ( string
                                 , list
                                 , path
                                 , bool
                                 , customString
                                 , customList
                                 , customPath
                                 , customBool
                                 ) where

import Language.Haskell.TH

import MSBuildGen.DSL.Core
import MSBuildGen.Types

string :: String -> DecsQ
string n = customString n n

list :: String -> DecsQ
list n = customList n n

path :: String -> DecsQ
path n = customPath n n

bool :: String -> DecsQ
bool n = customBool n n

customString :: String -> String -> DecsQ
customString = property 'String

customList :: String -> String -> DecsQ
customList = property 'List

customPath :: String -> String -> DecsQ
customPath = property 'Path

customBool :: String -> String -> DecsQ
customBool = property 'Bool

property :: Name -> String -> String -> DecsQ
property t n v = simpleData n $ instances t n v

instances :: Name -> String -> String -> DecsQ
instances t n v = [d| instance Property $(propType) where toMSBuildProperty _ = Property $(typeExp) $(propNameExp)
                      instance Value $(propType) where toMSBuildValue = toMSBuildValue . toMSBuildProperty
                      instance Condition $(propType) where toMSBuildCondition = toMSBuildCondition . toMSBuildProperty
                  |]
    where propType = conT $ mkName n
          propNameExp = litE $ stringL v
          typeExp = conE t
