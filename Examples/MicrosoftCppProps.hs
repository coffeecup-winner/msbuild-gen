module Examples.MicrosoftCppProps where

import MSBuildGen
import Examples.KnownProperties
import Examples.KnownItems

microsoft_cpp_props :: Project
microsoft_cpp_props = project "Microsoft.Cpp.props" $ do
    -- <!--
    -- ***********************************************************************************************
    -- Microsoft.Cpp.Props

    -- WARNING:  DO NOT MODIFY this file unless you are knowledgeable about MSBuild and have
    --           created a backup copy.  Incorrect changes to this file will make it
    --           impossible to load or build your projects from the command-line or the IDE.

    -- Copyright (C) Microsoft Corporation. All rights reserved.
    -- ***********************************************************************************************
    -- -->

    -- <!-- *******************************************************************************************
    --       Cpp settings mapping file
    --      ******************************************************************************************* -->
    -- <!-- Default values -->
    propertyGroup $ do
        TargetExt =: ""

    -- <!-- Specific values -->
    ConfigurationType === "Application" ? do
        propertyGroup $ do
            LinkCompiled =: True
            TargetExt =: ".exe"
            OutputType =: "exe"

    ConfigurationType === "DynamicLibrary" ? do
        propertyGroup $ do
            LinkCompiled =: True
            -- <!-- $(GenerateImportLib) should be set to true when you want to generate the import library as part of the BuildCompile pass rather than wait
            --      until the BuildLink pass for Linker to generate it. This allows circular dependencies between dlls to be satisfied when building using passes -->
            GenerateImportLib === True ? do
                ImpLibCompiled =: True
            TargetExt =: ".dll"
            OutputType =: "library"

    ConfigurationType === "StaticLibrary" ? do
        propertyGroup $ do
            LibCompiled =: True
            TargetExt =: ".lib"
            OutputType =: "staticlibrary"

    -- <!-- Default debugger properties -->
    propertyGroup $ do
        -- <!-- Local Windows debugger -->
        LocalDebuggerDebuggerType =: "Auto"
        -- <!-- LocalDebuggerCommand is defined into an empty $(TargetPath) for the property page, it is later redefined to its proper value. -->
        TargetPath !== "" ? do
            LocalDebuggerCommand =: TargetPath
        LocalDebuggerWorkingDirectory =: ProjectDir
        LocalDebuggerMergeEnvironment =: True
        LocalDebuggerAttach =: False
        LocalDebuggerSQLDebugging =: False

        -- <!-- Remote Windows debugger -->
        RemoteDebuggerDebuggerType =: "Auto"
        RemoteDebuggerWorkingDirectory =: ProjectDir
        RemoteDebuggerServerName =: COMPUTERNAME
        RemoteDebuggerConnection =: "RemoteWithAuthentication"
        RemoteDebuggerAttach =: False
        RemoteDebuggerSQLDebugging =: False

        -- <!-- MPI Cluster Debugger -->
        MpiDebuggerSchedulerNode =: "localhost/1"
        TargetPath !== "" ? do
            MpiDebuggerApplicationCommand =: TargetFileName
        MpiDebuggerDebuggerType =: "Auto"
        MpiDebuggerNetworkSecurityMode =: "AnyAddress"
        MpiDebuggerSchedulerTimeout =: "5000"
        MpiDebuggerDeployCommonRuntime =: True
        MpiDebuggerCleanupDeployment =: True

        -- <!-- Web Browser debugger -->
        WebBrowserDebuggerDebuggerType =: "Auto"

        -- <!-- Web Service debugger-->
        WebServiceDebuggerDebuggerType =: "Auto"
        WebServiceDebuggerSQLDebugging =: False

    propertyGroup $ do
        Language =: "C++"
        DefaultLanguageSourceExtension =: ".cpp"

    -- <!-- Import CL and Link defaults-->
    include (VCTargetsPath \\ "Microsoft.Cl.Common.props")
    include (VCTargetsPath \\ "Microsoft.Link.Common.props")
    exists (VCTargetsPath \\ "Microsoft.CodeAnalysis.props") ? do
        include (VCTargetsPath \\ "Microsoft.CodeAnalysis.props")

    -- <!-- Define defaults for all tools for property page-->
    propertyGroup $ do
        TargetName =? ProjectName
        ExtensionsToDeleteOnClean =? ["*.cdf", "*.cache", "*.obj", "*.ilk", "*.resources", "*.tlb", "*.tli", "*.tlh",
                                      "*.tmp", "*.rsp", "*.pgc", "*.pgd", "*.meta", "*.tlog", "*.manifest", "*.res",
                                      "*.pch", "*.exp", "*.idb", "*.rep", "*.xdc", "*.pdb", "*_manifest.rc", "*.bsc",
                                      "*.sbr", "*.xml", "*.metagen", "*.bi"]
        IncludeVersionInInteropName =? True
        InteropOutputPath =? "Interop\\"
        EmbedManifest =? True
        PreBuildEventUseInBuild =? True
        PreLinkEventUseInBuild =? True
        PostBuildEventUseInBuild =? True
        DocumentLibraryDependencies =? True

        -- <!-- CLR enabled -->
        EnableManagedIncrementalBuild === "" &&& CLRSupport !== "" &&& CLRSupport !== False ? do
            EnableManagedIncrementalBuild =: True
        EnableManagedIncrementalBuild =? False
        IgnoreImportLibrary === "" &&& CLRSupport !== "" &&& CLRSupport !== False ? do
            IgnoreImportLibrary =: True
        IgnoreImportLibrary =? False
        GenerateManifest =? True

    itemDefinitionGroup $ do
        item CustomBuild $ do
            Message =? "Performing Custom Build Tools"
        item CustomBuildStep $ do
            Message =? "Performing Custom Build Step"
        item Lib $ do
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            OutputFile =? OutDir <> TargetName <> TargetExt
            SuppressStartupBanner =? True
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
        item Midl $ do
            TypeLibraryName =? IntDir <> ProjectName <> ".tlb"
            TargetEnvironment =? "Win32"
            WarningLevel =? "1"
            DefaultCharType =? "Signed"
            SuppressStartupBanner =? True
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
            IgnoreStandardIncludePath =? False
            WarnAsError =? False
            GenerateTypeLibrary =? True
            ErrorCheckAllocations =? False
            ErrorCheckBounds =? False
            ErrorCheckEnumRange =? False
            ErrorCheckRefPointers =? False
            ErrorCheckStubData =? False
            StructMemberAlignment =? "NotSet"
            HeaderFileName =? Filename <> "_h.h"
        item ResourceCompile $ do
            ResourceOutputFileName =? IntDir <> Filename <> ".res"
            Culture =? "0x0409"
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
        item Manifest $ do
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            SuppressStartupBanner =? True
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
            VerboseOutput =? True
            GenerateCatalogFiles =? False
            UpdateFileHashes =? False
            EmbedManifest === False ? do
                OutputManifestFile =: TargetPath <> ".manifest"
            EmbedManifest === True &&& EmbedManifestBy === "LINK" ? do
                OutputManifestFile =: IntDir <> TargetName <> TargetExt <> ".embed.manifest"
        item ManifestResourceCompile $ do
            ResourceOutputFileName =? IntDir <> TargetName <> TargetExt <> ".embed.manifest.res"
        item XdcMake $ do
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            SuppressStartupBanner =? True
            OutputFile =? OutDir <> TargetName <> ".xml"
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
            UseUnicodeResponseFiles =? True
            ValidateIntelliSense =? False
        item BscMake $ do
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            SuppressStartupBanner =? True
            PreserveSBR =? False
            OutputFile =? OutDir <> TargetName <> ".bsc"
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
        item XSD $ do
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            SuppressStartupBanner =? True
            Language_Metadata =? "cpp"
            Namespace =? TargetName
            GenerateFromSchema =? "dataset"
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
        item CustomBuild $ do
            TrackerLogDirectory =? IntDir
            MinimalRebuildFromTracking =? True
            AcceptableNonZeroExitCodes_Metadata =? AcceptableNonZeroExitCodes
        item ProjectReference $ do
            LinkLibraryDependencies === "" &&& ConfigurationType === "StaticLibrary" ? do
                LinkLibraryDependencies =: False
            LinkLibraryDependencies =? True
            UseLibraryDependencyInputs =? False
            ReferenceOutputAssembly =? True
            Private =? True
        item Reference $ do
            ReferenceOutputAssembly =: True
            CopyLocalSatelliteAssemblies =: True
        item COMReference $ do
            ReferenceOutputAssembly =: True
            CopyLocalSatelliteAssemblies =: True

    itemGroup $ do
        -- <!--  Unique items for the project-->
        CustomBuildStep <: ProjectName
        PreBuildEvent <: ProjectName
        PreLinkEvent <: ProjectName
        PostBuildEvent <: ProjectName

    -- <!-- Import Platform specific settings -->
    Platform !== "" &&& exists (VCTargetsPath \\ "Platforms" \\ Platform \\ "Microsoft.Cpp." <> Platform <> ".props") ? do
        include (VCTargetsPath \\ "Platforms" \\ Platform \\ "Microsoft.Cpp." <> Platform <> ".props")
