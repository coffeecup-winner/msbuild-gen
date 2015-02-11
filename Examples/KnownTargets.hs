{-# LANGUAGE TemplateHaskell #-}
module Examples.KnownTargets where

import MSBuildGen.DSL.Targets

target "InvalidPlatformError"

task "VCMessage"
string "Code"
string "Type"
list "Arguments"
