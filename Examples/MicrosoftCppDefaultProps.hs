module Examples.MicrosoftCppDefaultProps where

import MSBuildGen
import Examples.KnownProperties
import Examples.KnownItems

microsoft_cpp_default_props :: Project
microsoft_cpp_default_props = project "Microsoft.Cpp.Default.props" $ do
    -- <!--
    -- ***********************************************************************************************
    -- Microsoft.Cpp.Default.props

    -- WARNING:  DO NOT MODIFY this file unless you are knowledgeable about MSBuild and have
    --           created a backup copy.  Incorrect changes to this file will make it
    --           impossible to load or build your projects from the command-line or the IDE.

    -- Copyright (C) Microsoft Corporation. All rights reserved.
    -- ***********************************************************************************************
    -- -->

    propertyGroup $ do
        Platform =? "Win32"
    --     <!-- When the Framework Version is <= 3.5, set PlatformToolset to use the 9.0 toolset.
    --          When the Framework Version is >= 4.0, set PlatformToolset to use the 10.0 toolset.
    --          If Targetframeworkversion does not exist, then leave the default toolset -->
        ("'$(PlatformToolset)' == '' and ('$(TargetFrameworkVersion)' == 'v3.5' or '$(TargetFrameworkVersion)' == 'v3.0' or '$(TargetFrameworkVersion)' == 'v2.0')") ? do
            PlatformToolset =: "v90"

    --   <!-- This is the Cpp defaults settings mapping file. It defines all the project properties values
    --        (equivalent of System Macros) and also all the ItemDefinitionGroup defaults for each known
    --        ItemGroup (known as the default of the defaults in the current Project System) -->

    propertyGroup $ do
        Configuration =? "Debug"
        ConfigurationType =? "Application"

    --   <!-- Allow platforms to define the defaults first -->
    exists (VCTargetsPath \\ "Platforms" \\ Platform \\ "Microsoft.Cpp.$(Platform).default.props") ? do
        include (VCTargetsPath \\ "Platforms" \\ Platform \\ "Microsoft.Cpp.$(Platform).default.props")

    --   <!-- Default OutputPath -->
    propertyGroup $ do
        OutputPath =? OutDir
        ("'$(OutputPath)' != '' and !HasTrailingSlash('$(OutputPath)')") ? do
            OutputPath =: "$(OutputPath)\\"

    propertyGroup $ do
        ProjectName =? MSBuildProjectName
        (TargetName === "" &&& AssemblyName !== "") ? do
            TargetName =: AssemblyName
        ProjectFileName =? MSBuildProjectFile
        ProjectExt =? MSBuildProjectExtension

        ProjectDir =? MSBuildProjectDirectory
        ProjectPath =? "$(ProjectDir)$(ProjectFileName)"
        PlatformName =? Platform
        SolutionDir =? ProjectDir

        UserRootDir =? (LocalAppData \\ "Microsoft" \\ "MSBuild" \\ "v4.0")

        MSBuildAllProjects =: "$(MSBuildAllProjects);$(MSBuildProjectFullPath);$(MSBuildToolsPath)\\Microsoft.Common.targets"
        exists ("$(MSBuildProjectFullPath).user") ? do
            MSBuildAllProjects =: "$(MSBuildAllProjects);$(MSBuildProjectFullPath).user"

        WholeProgramOptimizationDelayImport =? True
        WholeProgramOptimization =? False
        CLRSupport =? False
        UseOfATL =? False
        UseOfMFC =? False
        SQLDebugging =? False
        Attach =? False

    itemDefinitionGroup $ do
        item BuildLog $ do
            Path_Metadata =: "$(IntDir)\\$(MSBuildProjectName).log"
