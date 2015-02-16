{-# LANGUAGE TemplateHaskell #-}
module MSBuildGen.DSL.Targets ( target
                              , customTarget
                              , task
                              , string
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

target :: String -> DecsQ
target n = customTarget n n

customTarget :: String -> String -> DecsQ
customTarget n v = simpleData n $ targetInstances n v

targetInstances :: String -> String -> DecsQ
targetInstances n v = [d| instance Target $(targetType) where toMSBuildTarget _ = Target $(targetNameExp) |]
    where targetType = conT $ mkName n
          targetNameExp = litE $ stringL v

task :: String -> DecsQ
task n = simpleData n $ taskInstances n

taskInstances :: String -> DecsQ
taskInstances n = [d| instance Task $(taskType) where toMSBuildTask _ = Task $(taskNameExp) |]
    where taskType = conT $ mkName n
          taskNameExp = litE $ stringL n

string :: String -> DecsQ
string n = customString n n

list :: String -> DecsQ
list n = customList n n

path :: String -> DecsQ
path n = customPath n n

bool :: String -> DecsQ
bool n = customBool n n

customString :: String -> String -> DecsQ
customString = param 'String

customList :: String -> String -> DecsQ
customList = param 'List

customPath :: String -> String -> DecsQ
customPath = param 'Path

customBool :: String -> String -> DecsQ
customBool = param 'Bool

param :: Name -> String -> String -> DecsQ
param t n v = simpleData n $ paramInstances t n v

paramInstances :: Name -> String -> String -> DecsQ
paramInstances t n v = [d| instance TaskParameter $(paramType) where toMSBuildTaskParameter _ = TaskParameter $(typeExp) $(paramNameExp) |]
    where paramType = conT $ mkName n
          paramNameExp = litE $ stringL v
          typeExp = conE t
