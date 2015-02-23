{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Examples.KnownTargets where

import MSBuildGen.DSL.Targets

target "InvalidPlatformError"
target "GenerateTargetFrameworkMonikerAttribute"
target "Build"
target "Rebuild"
customTarget "PrepareForBuild" "_PrepareForBuild"
customTarget "PrepareForRebuild" "_PrepareForRebuild"
customTarget "PrepareForClean" "_PrepareForClean"
target "AfterBuild"
target "BeforeBuildGenerateSources"
target "AfterBuildGenerateSources"
target "BeforeBuildCompile"
target "AfterBuildCompile"
target "LibLinkOnly"
target "GetClCompile"
target "GetResourceCompile"
target "GetMidl"
target "GetImpLib"
target "GetLib"
target "GetLink"
target "GetCustomBuild"
target "GetXsd"
target "GetXdcMake"
target "GetBscMake"

task "VCMessage"
string "Code"
string "Type"
list "Arguments"

task "Delete"
path "Files"

task "WriteLinesToFile"
path "File"
string "Lines"
bool "ContinueOnError"
bool "Overwrite"
