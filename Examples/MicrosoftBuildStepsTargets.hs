module Examples.MicrosoftBuildStepsTargets where

import MSBuildGen
import Examples.KnownProperties
import Examples.KnownItems
import Examples.KnownTargets

microsoft_build_steps_targets :: Project
microsoft_build_steps_targets = project "Microsoft.BuildSteps.targets" $ do
    -- <!--
    -- ***********************************************************************************************
    -- Microsoft.BuildSteps.targets

    -- WARNING:  DO NOT MODIFY this file unless you are knowledgeable about MSBuild and have
    --           created a backup copy.  Incorrect changes to this file will make it
    --           impossible to load or build your projects from the command-line or the IDE.

    -- This file defines the steps in the standard build process for C++ projects.  It
    -- contains all the steps that are specific to building Visual C++ projects.

    -- Copyright (C) Microsoft Corporation. All rights reserved.

    -- ***********************************************************************************************
    -- -->

    propertyGroup $ do
        --   <!-- CLRSupport = pure, safe, true etc are supported -->
        CLRSupport === "" ||| CLRSupport === "false" ||| CLRSupport === "oldsyntax" ? do
            GenerateTargetFrameworkAttribute =: False
        --   <!-- By default we do not want to build project to project references if they are disabled in the solution configuration -->
        OnlyReferenceAndBuildProjectsEnabledInSolutionConfiguration =? True

    include (MSBuildToolsPath \\ "Microsoft.Common.Targets")

    --     <!--
    --     ============================================================
    --                                         GenerateTargetFrameworkMonikerAttribute

    --     Emit any specified code fragments into a temporary source file for the compiler.
    --     Overridden for C++ special care is needed when using precompiled headers.
    --     ============================================================
    --     -->

    GenerateTargetFrameworkAttribute === True ? do
        target GenerateTargetFrameworkMonikerAttribute $ do
            before "ClCompile"
            dependsOn ["PrepareForBuild", "GetReferenceAssemblyPaths"]
            inputs (VCTargetsPath \\ "Microsoft.BuildSteps.targets")
            outputs TargetFrameworkMonikerAssemblyAttributesPath

            propertyGroup $ do
                -- <!-- This attribute is only available in mscorlib v4 and later -->
                GenerateTargetFrameworkAttribute === True &&& TargetingClr2Framework !== True ? do
                    AdditionalSourcesText =: "$(AdditionalSourcesText)\r\n"
                                          ++ "#using &lt;mscorlib.dll&gt;\r\n"
                                          ++ "[assembly: System::Runtime::Versioning::TargetFrameworkAttribute(L&quot;$(TargetFrameworkMoniker)&quot;, FrameworkDisplayName=L&quot;$(TargetFrameworkMonikerDisplayName)&quot;)]%3b"

            -- <!-- This is a file shared between projects so we have to take care to handle simultaneous writes (by ContinueOnError)
            --      and a race between clean from one project and build from another (by not adding to FilesWritten so it doesn't clean) -->
            AdditionalSourcesText !== "" ? do
                run WriteLinesToFile $ do
                    File =: TargetFrameworkMonikerAssemblyAttributesPath
                    Lines =: AdditionalSourcesText
                    ContinueOnError =: True
                    Overwrite =: True

            itemGroup $ do
                ClCompile !== "" &&& AdditionalSourcesText !== "" ? do
                    ClCompile <:! TargetFrameworkMonikerAssemblyAttributesPath $ do
                        -- <!-- Since we didn't emit any necessary #include "stdafx.h" or similar -->
                        PrecompiledHeader =: "NotUsing"
                        -- <!-- The appropriate CompileAsManaged setting will automatically be set depending on the current CLRSupport value -->

    --   <!-- CPP build is done in 3 steps:
    --           1. Generate sources (BuildGenerateSources)
    --           2. Compile          (BuildCompile)
    --           3. Link             (BuildLink)
    --        Each step can be executed separately. Populate BuildSteps with one or more steps
    --        before importing this project to build only particular steps. -->
    propertyGroup $ do
        BuildSteps =? [ "SetBuildDefaultEnvironmentVariables"
                      , "SetUserMacroEnvironmentVariables"
                      , "ResolveReferences"
                      , "PrepareForBuild"
                      , "InitializeBuildStatus"
                      , "BuildGenerateSources"
                      , "BuildCompile"
                      , "BuildLink"
                      ]

        BuildDependsOn =: [ "_PrepareForBuild"
                          , "$(BuildSteps)"
                          , "AfterBuild"
                          , "FinalizeBuildStatus"
                          ]

        RebuildDependsOn =: [ "_PrepareForRebuild"
                            , "$(RebuildDependsOn)"
                            ]

        CleanDependsOn =: [ "_PrepareForClean"
                          , "$(CleanDependsOn)"
                          ]

    "'$(_InvalidConfigurationWarning)' != 'true'" ? do
        target Build $ do
            dependsOn ["_DetermineManagedStateFromCL", "$(BuildDependsOn)"]
            itemGroup $ do
                ManagedAssembly === True ? do
                    ManagedTargetPath <: TargetPath
            returns ManagedTargetPath

    "'$(_InvalidConfigurationWarning)' != 'true'" ? do
        target Rebuild $ do
            dependsOn ["_DetermineManagedStateFromCL", "$(RebuildDependsOn)"]
            itemGroup $ do
                ManagedAssembly === True ? do
                    ManagedTargetPath <: TargetPath
            returns ManagedTargetPath

    target PrepareForBuild $ do
        propertyGroup $ do
            BuildType =? "Build"

        itemGroup $ do
            -- <!-- We only want to run our targets for vcxproj, not csproj.  This allows our us to target 3.5 toolset.-->
            ExcludedFromBuild !== True &&& (Extension === ".obj" ||| Extension === ".lib") ? do
                Link <: CustomBuild
            ExcludedFromBuild !== True &&& (Extension === ".obj" ||| Extension === ".lib") ? do
                Lib <: CustomBuild

            -- <!-- Remove CustomBuild ItemGroup that doesn't meet the condition. This prevents showing "Skipping target "CustomBuild" because it has no outputs. -->
            Command === "" ? do
                CustomBuildStep </: CustomBuildStep
            Command === "" ||| ExcludedFromBuild === True ? do
                CustomBuild </: CustomBuild

    target PrepareForRebuild $ do
        propertyGroup $ do
            BuildType =? "Rebuild"
            "%(ClCompile.GenerateXMLDocumentationFiles) == 'true'" ? do
                ClCompileGenerateXMLDocumentationFiles =: True
        -- <!-- We only want to run our targets for vcxproj, not csproj.  This allows our us to target 3.5 toolset.-->
        itemGroup $ do
            Command === "" ? do
                CustomBuildStep </: CustomBuildStep
            Command === "" ||| ExcludedFromBuild === True ? do
                CustomBuild </: CustomBuild

    target PrepareForClean $ do
        propertyGroup $ do
            BuildType =? "Clean"
        exists LastBuildState ? do
            run Delete $ Files =: LastBuildState

    target AfterBuild $ return ()

    propertyGroup $ do
        TLogLocation =: IntDir
        LastBuildUnsuccessful =: IntDir <> ProjectName <> ".unsuccessfulbuild"
        LastBuildState =: IntDir <> ProjectName <> ".lastbuildstate"


    --   <!-- *******************************************************************************************
    --         BeforeBuildGenerateSources
    --        ******************************************************************************************* -->
    --   <!-- Redefine this target in your project in order to run tasks just before BuildGenerateSources -->
    target BeforeBuildGenerateSources $ return ()

    propertyGroup $ do
        BeforeBuildGenerateSourcesTargets =: [ "$(BeforeBuildGenerateSourcesTargets)"
                                             , "BeforeBuildGenerateSources"
                                             , ""
                                             ]

        AfterBuildGenerateSourcesTargets =: [ "$(AfterBuildGenerateSourcesTargets)"
                                            , "AfterBuildGenerateSources"
                                            , ""
                                            ]

    --   <!-- *******************************************************************************************
    --         AfterBuildGenerateSources
    --        ******************************************************************************************* -->
    --   <!-- Redefine this target in your project in order to run tasks just after BuildGenerateSources -->
    target AfterBuildGenerateSources $ return ()

    --   <!-- *******************************************************************************************
    --         BeforeBuildCompile
    --        ******************************************************************************************* -->
    --   <!-- Redefine this target in your project in order to run tasks just before BuildCompile -->
    target BeforeBuildCompile $ return ()

    --   <!-- *******************************************************************************************
    --         AfterBuildCompile
    --        ******************************************************************************************* -->
    --   <!-- Redefine this target in your project in order to run tasks just after BuildCompile -->
    target AfterBuildCompile $ return ()

    target LibLinkOnly $ dependsOn ["$(CommonBuildOnlyTargets)", "$(BeforeBuildLinkTargets)", "$(BuildLibTargets)", "$(BuildLinkTargets)", "$(AfterBuildLinkTargets)"]

    --   <!-- *******************************************************************************************
    --           Utility targets to get files from project from another project using the MSBuild task
    --        ******************************************************************************************* -->
    target GetClCompile $ returns ClCompile
    target GetResourceCompile $ returns ResourceCompile
    target GetMidl $ returns Midl
    target GetImpLib $ returns ImpLib
    target GetLib $ returns Lib
    target GetLink $ returns Link
    target GetCustomBuild $ returns CustomBuild
    target GetXsd $ returns Xsd
    target GetXdcMake $ returns XdcMake
    target GetBscMake $ returns BscMake
