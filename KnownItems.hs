module KnownItems where

import MSBuildGen.Types

data KnownItem = CustomBuild
               | CustomBuildStep
               | Lib
               | Midl
               | ResourceCompile
               | Manifest
               | ManifestResourceCompile
               | XdcMake
               | BscMake
               | XSD
               | ProjectReference
               | Reference
               | COMReference

instance Item KnownItem where
    toMSBuildItem CustomBuild = Item "CustomBuild"
    toMSBuildItem CustomBuildStep = Item "CustomBuildStep"
    toMSBuildItem Lib = Item "Lib"
    toMSBuildItem Midl = Item "Midl"
    toMSBuildItem ResourceCompile = Item "ResourceCompile"
    toMSBuildItem Manifest = Item "Manifest"
    toMSBuildItem ManifestResourceCompile = Item "ManifestResourceCompile"
    toMSBuildItem XdcMake = Item "XdcMake"
    toMSBuildItem BscMake = Item "BscMake"
    toMSBuildItem XSD = Item "XSD"
    toMSBuildItem ProjectReference = Item "ProjectReference"
    toMSBuildItem Reference = Item "Reference"
    toMSBuildItem COMReference = Item "COMReference"

data KnownItemMetadata = Message
                       | TrackerLogDirectory
                       | MinimalRebuildFromTracking
                       | OutputFile
                       | SuppressStartupBanner
                       | AcceptableNonZeroExitCodes
                       | TypeLibraryName
                       | TargetEnvironment
                       | WarningLevel
                       | DefaultCharType
                       | IgnoreStandardIncludePath
                       | WarnAsError
                       | GenerateTypeLibrary
                       | ErrorCheckAllocations
                       | ErrorCheckBounds
                       | ErrorCheckEnumRange
                       | ErrorCheckRefPointers
                       | ErrorCheckStubData
                       | StructMemberAlignment
                       | HeaderFileName
                       | ResourceOutputFileName
                       | Culture
                       | VerboseOutput
                       | GenerateCatalogFiles
                       | UpdateFileHashes
                       | OutputManifestFile
                       | UseUnicodeResponseFiles
                       | ValidateIntelliSense
                       | PreserveSBR
                       | Namespace
                       | GenerateFromSchema
                       | LinkLibraryDependencies
                       | UseLibraryDependencyInputs
                       | ReferenceOutputAssembly
                       | Private
                       | CopyLocalSatelliteAssemblies
                       | Language_Metadata

instance ItemMetadata KnownItemMetadata where
    toMSBuildItemMetadata Message = ItemMetadata String "Message"
    toMSBuildItemMetadata TrackerLogDirectory = ItemMetadata Path "TrackerLogDirectory"
    toMSBuildItemMetadata MinimalRebuildFromTracking = ItemMetadata Bool "MinimalRebuildFromTracking"
    toMSBuildItemMetadata OutputFile = ItemMetadata Path "OutputFile"
    toMSBuildItemMetadata SuppressStartupBanner = ItemMetadata Bool "SuppressStartupBanner"
    toMSBuildItemMetadata AcceptableNonZeroExitCodes = ItemMetadata List "AcceptableNonZeroExitCodes"
    toMSBuildItemMetadata TypeLibraryName = ItemMetadata Path "TypeLibraryName"
    toMSBuildItemMetadata TargetEnvironment = ItemMetadata String "TargetEnvironment"
    toMSBuildItemMetadata WarningLevel = ItemMetadata String "WarningLevel"
    toMSBuildItemMetadata DefaultCharType = ItemMetadata String "DefaultCharType"
    toMSBuildItemMetadata IgnoreStandardIncludePath = ItemMetadata Bool "IgnoreStandardIncludePath"
    toMSBuildItemMetadata WarnAsError = ItemMetadata Bool "WarnAsError"
    toMSBuildItemMetadata GenerateTypeLibrary = ItemMetadata Bool "GenerateTypeLibrary"
    toMSBuildItemMetadata ErrorCheckAllocations = ItemMetadata Bool "ErrorCheckAllocations"
    toMSBuildItemMetadata ErrorCheckBounds = ItemMetadata Bool "ErrorCheckBounds"
    toMSBuildItemMetadata ErrorCheckEnumRange = ItemMetadata Bool "ErrorCheckEnumRange"
    toMSBuildItemMetadata ErrorCheckRefPointers = ItemMetadata Bool "ErrorCheckRefPointers"
    toMSBuildItemMetadata ErrorCheckStubData = ItemMetadata Bool "ErrorCheckStubData"
    toMSBuildItemMetadata StructMemberAlignment = ItemMetadata String "StructMemberAlignment"
    toMSBuildItemMetadata HeaderFileName = ItemMetadata Path "HeaderFileName"
    toMSBuildItemMetadata ResourceOutputFileName = ItemMetadata Path "ResourceOutputFileName"
    toMSBuildItemMetadata Culture = ItemMetadata String "Culture"
    toMSBuildItemMetadata VerboseOutput = ItemMetadata Bool "VerboseOutput"
    toMSBuildItemMetadata GenerateCatalogFiles = ItemMetadata Bool "GenerateCatalogFiles"
    toMSBuildItemMetadata UpdateFileHashes = ItemMetadata Bool "UpdateFileHashes"
    toMSBuildItemMetadata OutputManifestFile = ItemMetadata Path "OutputManifestFile"
    toMSBuildItemMetadata UseUnicodeResponseFiles = ItemMetadata Bool "UseUnicodeResponseFiles"
    toMSBuildItemMetadata ValidateIntelliSense = ItemMetadata Bool "ValidateIntelliSense"
    toMSBuildItemMetadata PreserveSBR = ItemMetadata Bool "PreserveSBR"
    toMSBuildItemMetadata Namespace = ItemMetadata String "Namespace"
    toMSBuildItemMetadata GenerateFromSchema = ItemMetadata String "GenerateFromSchema"
    toMSBuildItemMetadata LinkLibraryDependencies = ItemMetadata Bool "LinkLibraryDependencies"
    toMSBuildItemMetadata UseLibraryDependencyInputs = ItemMetadata Bool "UseLibraryDependencyInputs"
    toMSBuildItemMetadata ReferenceOutputAssembly = ItemMetadata Bool "ReferenceOutputAssembly"
    toMSBuildItemMetadata Private = ItemMetadata Bool "Private"
    toMSBuildItemMetadata CopyLocalSatelliteAssemblies = ItemMetadata Bool "CopyLocalSatelliteAssemblies"
    toMSBuildItemMetadata Language_Metadata = ItemMetadata String "Language"

instance Condition KnownItemMetadata where
    toMSBuildCondition = toMSBuildCondition . toMSBuildItemMetadata
