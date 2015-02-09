module KnownProperties where

import MSBuildGen.Types

data KnownProperty = TargetExt
                   | ConfigurationType
                   | LinkCompiled
                   | OutputType
                   | GenerateImportLib
                   | ImpLibCompiled
                   | LibCompiled
                   | LocalDebuggerDebuggerType
                   | TargetPath
                   | LocalDebuggerCommand
                   | LocalDebuggerWorkingDirectory
                   | ProjectDir
                   | LocalDebuggerMergeEnvironment
                   | LocalDebuggerAttach
                   | LocalDebuggerSQLDebugging
                   | RemoteDebuggerDebuggerType
                   | RemoteDebuggerWorkingDirectory
                   | RemoteDebuggerServerName
                   | COMPUTERNAME
                   | RemoteDebuggerConnection
                   | RemoteDebuggerAttach
                   | RemoteDebuggerSQLDebugging
                   | MpiDebuggerSchedulerNode
                   | MpiDebuggerApplicationCommand
                   | TargetFileName
                   | MpiDebuggerDebuggerType
                   | MpiDebuggerNetworkSecurityMode
                   | MpiDebuggerSchedulerTimeout
                   | MpiDebuggerDeployCommonRuntime
                   | MpiDebuggerCleanupDeployment
                   | WebBrowserDebuggerDebuggerType
                   | WebServiceDebuggerDebuggerType
                   | WebServiceDebuggerSQLDebugging
                   | Language
                   | DefaultLanguageSourceExtension
                   | VCTargetsPath
                   | TargetName
                   | ProjectName
                   | ExtensionsToDeleteOnClean
                   | IncludeVersionInInteropName
                   | InteropOutputPath
                   | EmbedManifest
                   | PreBuildEventUseInBuild
                   | PreLinkEventUseInBuild
                   | PostBuildEventUseInBuild
                   | DocumentLibraryDependencies
                   | EnableManagedIncrementalBuild
                   | CLRSupport
                   | IgnoreImportLibrary
                   | GenerateManifest
                   | Platform
                   | IntDir
                   | EmbedManifestBy

instance Property KnownProperty where
    toMSBuildProperty TargetExt = Property String "TargetExt"
    toMSBuildProperty ConfigurationType = Property String "ConfigurationType"
    toMSBuildProperty LinkCompiled = Property Bool "LinkCompiled"
    toMSBuildProperty OutputType = Property String "OutputType"
    toMSBuildProperty GenerateImportLib = Property Bool "GenerateImportLib"
    toMSBuildProperty ImpLibCompiled = Property Bool "ImpLibCompiled"
    toMSBuildProperty LibCompiled = Property Bool "LibCompiled"
    toMSBuildProperty LocalDebuggerDebuggerType = Property String "LocalDebuggerDebuggerType"
    toMSBuildProperty TargetPath = Property Path "TargetPath"
    toMSBuildProperty LocalDebuggerCommand = Property String "LocalDebuggerCommand"
    toMSBuildProperty LocalDebuggerWorkingDirectory = Property Path "LocalDebuggerWorkingDirectory"
    toMSBuildProperty ProjectDir = Property Path "ProjectDir"
    toMSBuildProperty LocalDebuggerMergeEnvironment = Property Bool "LocalDebuggerMergeEnvironment"
    toMSBuildProperty LocalDebuggerAttach = Property Bool "LocalDebuggerAttach"
    toMSBuildProperty LocalDebuggerSQLDebugging = Property Bool "LocalDebuggerSQLDebugging"
    toMSBuildProperty RemoteDebuggerDebuggerType = Property String "RemoteDebuggerDebuggerType"
    toMSBuildProperty RemoteDebuggerWorkingDirectory = Property Path "RemoteDebuggerWorkingDirectory"
    toMSBuildProperty RemoteDebuggerServerName = Property String "RemoteDebuggerServerName"
    toMSBuildProperty COMPUTERNAME = Property String "COMPUTERNAME"
    toMSBuildProperty RemoteDebuggerConnection = Property String "RemoteDebuggerConnection"
    toMSBuildProperty RemoteDebuggerAttach = Property Bool "RemoteDebuggerAttach"
    toMSBuildProperty RemoteDebuggerSQLDebugging = Property Bool "RemoteDebuggerSQLDebugging"
    toMSBuildProperty MpiDebuggerSchedulerNode = Property String "MpiDebuggerSchedulerNode"
    toMSBuildProperty MpiDebuggerApplicationCommand = Property String "MpiDebuggerApplicationCommand"
    toMSBuildProperty TargetFileName = Property Path "TargetFileName"
    toMSBuildProperty MpiDebuggerDebuggerType = Property String "MpiDebuggerDebuggerType"
    toMSBuildProperty MpiDebuggerNetworkSecurityMode = Property String "MpiDebuggerNetworkSecurityMode"
    toMSBuildProperty MpiDebuggerSchedulerTimeout = Property String "MpiDebuggerSchedulerTimeout"
    toMSBuildProperty MpiDebuggerDeployCommonRuntime = Property Bool "MpiDebuggerDeployCommonRuntime"
    toMSBuildProperty MpiDebuggerCleanupDeployment = Property Bool "MpiDebuggerCleanupDeployment"
    toMSBuildProperty WebBrowserDebuggerDebuggerType = Property String "WebBrowserDebuggerDebuggerType"
    toMSBuildProperty WebServiceDebuggerDebuggerType = Property String "WebServiceDebuggerDebuggerType"
    toMSBuildProperty WebServiceDebuggerSQLDebugging = Property Bool "WebServiceDebuggerSQLDebugging"
    toMSBuildProperty Language = Property String "Language"
    toMSBuildProperty DefaultLanguageSourceExtension = Property String "DefaultLanguageSourceExtension"
    toMSBuildProperty VCTargetsPath = Property Path "VCTargetsPath"
    toMSBuildProperty TargetName = Property Path "TargetName"
    toMSBuildProperty ProjectName = Property Path "ProjectName"
    toMSBuildProperty ExtensionsToDeleteOnClean = Property List "ExtensionsToDeleteOnClean"
    toMSBuildProperty IncludeVersionInInteropName = Property Bool "IncludeVersionInInteropName"
    toMSBuildProperty InteropOutputPath = Property Path "InteropOutputPath"
    toMSBuildProperty EmbedManifest = Property Bool "EmbedManifest"
    toMSBuildProperty PreBuildEventUseInBuild = Property Bool "PreBuildEventUseInBuild"
    toMSBuildProperty PreLinkEventUseInBuild = Property Bool "PreLinkEventUseInBuild"
    toMSBuildProperty PostBuildEventUseInBuild = Property Bool "PostBuildEventUseInBuild"
    toMSBuildProperty DocumentLibraryDependencies = Property Bool "DocumentLibraryDependencies"
    toMSBuildProperty EnableManagedIncrementalBuild = Property Bool "EnableManagedIncrementalBuild"
    toMSBuildProperty CLRSupport = Property Bool "CLRSupport"
    toMSBuildProperty IgnoreImportLibrary = Property Bool "IgnoreImportLibrary"
    toMSBuildProperty GenerateManifest = Property Bool "GenerateManifest"
    toMSBuildProperty Platform = Property String "Platform"
    toMSBuildProperty IntDir = Property Path "IntDir"
    toMSBuildProperty EmbedManifestBy = Property String "EmbedManifestBy"

instance Value KnownProperty where
    toMSBuildValue = toMSBuildValue . toMSBuildProperty

instance Condition KnownProperty where
    toMSBuildCondition = toMSBuildCondition . toMSBuildProperty
