{-# LANGUAGE TemplateHaskell #-}
module MSBuildGen.DSL.Properties ( string
                                 , list
                                 , path
                                 , bool
                                 ) where

import Language.Haskell.TH

import MSBuildGen.DSL.Core
import MSBuildGen.Types

string :: String -> DecsQ
string = property 'String

list :: String -> DecsQ
list = property 'List

path :: String -> DecsQ
path = property 'Path

bool :: String -> DecsQ
bool = property 'Bool

property :: Name -> String -> DecsQ
property t n = simpleData n $ instances t n

instances :: Name -> String -> DecsQ
instances t n = [d| instance Property $(propType) where toMSBuildProperty _ = Property $(typeExp) $(propNameExp)
                    instance Value $(propType) where toMSBuildValue = toMSBuildValue . toMSBuildProperty
                    instance Condition $(propType) where toMSBuildCondition = toMSBuildCondition . toMSBuildProperty
                |]
    where propType = conT $ mkName n
          propNameExp = litE $ stringL n
          typeExp = conE t
