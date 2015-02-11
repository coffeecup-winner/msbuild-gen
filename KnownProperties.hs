{-# LANGUAGE TemplateHaskell #-}
module KnownProperties where

import MSBuildGen.DSL.Properties

string "TargetExt"
string "ConfigurationType"
bool "LinkCompiled"
string "OutputType"
bool "GenerateImportLib"
bool "ImpLibCompiled"
bool "LibCompiled"
string "LocalDebuggerDebuggerType"
path "TargetPath"
string "LocalDebuggerCommand"
path "LocalDebuggerWorkingDirectory"
path "ProjectDir"
bool "LocalDebuggerMergeEnvironment"
bool "LocalDebuggerAttach"
bool "LocalDebuggerSQLDebugging"
string "RemoteDebuggerDebuggerType"
path "RemoteDebuggerWorkingDirectory"
string "RemoteDebuggerServerName"
string "COMPUTERNAME"
string "RemoteDebuggerConnection"
bool "RemoteDebuggerAttach"
bool "RemoteDebuggerSQLDebugging"
string "MpiDebuggerSchedulerNode"
string "MpiDebuggerApplicationCommand"
path "TargetFileName"
string "MpiDebuggerDebuggerType"
string "MpiDebuggerNetworkSecurityMode"
string "MpiDebuggerSchedulerTimeout"
bool "MpiDebuggerDeployCommonRuntime"
bool "MpiDebuggerCleanupDeployment"
string "WebBrowserDebuggerDebuggerType"
string "WebServiceDebuggerDebuggerType"
bool "WebServiceDebuggerSQLDebugging"
string "Language"
string "DefaultLanguageSourceExtension"
path "VCTargetsPath"
path "TargetName"
path "ProjectName"
list "ExtensionsToDeleteOnClean"
bool "IncludeVersionInInteropName"
path "InteropOutputPath"
bool "EmbedManifest"
bool "PreBuildEventUseInBuild"
bool "PreLinkEventUseInBuild"
bool "PostBuildEventUseInBuild"
bool "DocumentLibraryDependencies"
bool "EnableManagedIncrementalBuild"
bool "CLRSupport"
bool "IgnoreImportLibrary"
bool "GenerateManifest"
string "Platform"
path "IntDir"
string "EmbedManifestBy"
list "AcceptableNonZeroExitCodes"
