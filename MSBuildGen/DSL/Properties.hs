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
customString = property ''TString

customList :: String -> String -> DecsQ
customList = property ''TList

customPath :: String -> String -> DecsQ
customPath = property ''TPath

customBool :: String -> String -> DecsQ
customBool = property ''TBool

property :: Name -> String -> String -> DecsQ
property t n v = simpleData n $ instances t n v

instances :: Name -> String -> String -> DecsQ
instances t n v = [d| instance Property $(propType) where toMSBuildProperty _ = Property $(propNameExp)
                      instance Value $(propType) where toMSBuildValue = toMSBuildValue . toMSBuildProperty
                      instance Path $(propType) where toMSBuildPath = toMSBuildPath . toMSBuildProperty
                      instance Condition $(propType) where toMSBuildCondition = toMSBuildCondition . toMSBuildProperty
                      type instance TypeOf $(propType) = $(typeType)
                  |]
    where propType = conT $ mkName n
          propNameExp = litE $ stringL v
          typeType = conT t
