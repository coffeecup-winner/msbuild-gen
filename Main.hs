module Main where

import MSBuildGen
import KnownProperties

main :: IO ()
main = writeProject microsoft_cpp_props

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
    (ConfigurationType === "Application") ? do
        propertyGroup $ do
            LinkCompiled =: True
            TargetExt =: ".exe"
            OutputType =: "exe"

    (ConfigurationType === "DynamicLibrary") ? do
        propertyGroup $ do
            LinkCompiled =: True
            -- <!-- $(GenerateImportLib) should be set to true when you want to generate the import library as part of the BuildCompile pass rather than wait
            --      until the BuildLink pass for Linker to generate it. This allows circular dependencies between dlls to be satisfied when building using passes -->
            (GenerateImportLib === "true") ? do
                ImpLibCompiled =: True
            TargetExt =: ".dll"
            OutputType =: "library"

    (ConfigurationType === "StaticLibrary") ? do
        propertyGroup $ do
            LibCompiled =: True
            TargetExt =: ".lib"
            OutputType =: "staticlibrary"

    -- <!-- Default debugger properties -->
    propertyGroup $ do
        -- <!-- Local Windows debugger -->
        LocalDebuggerDebuggerType =: "Auto"
        -- <!-- LocalDebuggerCommand is defined into an empty $(TargetPath) for the property page, it is later redefined to its proper value. -->
        (TargetPath !== "") ? do
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
        (TargetPath !== "") ? do
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
        (EnableManagedIncrementalBuild === "" &&& CLRSupport !== "" &&& CLRSupport !== "false") ? do
            EnableManagedIncrementalBuild =: True
        EnableManagedIncrementalBuild =? False
        (IgnoreImportLibrary === "" &&& CLRSupport !== "" &&& CLRSupport !== "false") ? do
            IgnoreImportLibrary =: True
        IgnoreImportLibrary =? False
        GenerateManifest =? True

    -- <ItemDefinitionGroup>
    --     <CustomBuild>
    --         <Message Condition="'%(CustomBuild.Message)'==''" >Performing Custom Build Tools</Message>
    --     </CustomBuild>
    --     <CustomBuildStep>
    --         <Message Condition="'%(CustomBuildStep.Message)'==''" >Performing Custom Build Step</Message>
    --     </CustomBuildStep>
    --     <Lib>
    --         <TrackerLogDirectory         Condition="'%(Lib.TrackerLogDirectory)'          == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking  Condition="'%(Lib.MinimalRebuildFromTracking)'   == ''">true</MinimalRebuildFromTracking>
    --         <OutputFile                  Condition="'%(Lib.OutputFile)'                   == ''">$(OutDir)$(TargetName)$(TargetExt)</OutputFile>
    --         <SuppressStartupBanner       Condition="'%(Lib.SuppressStartupBanner)'        == ''">true</SuppressStartupBanner>
    --         <AcceptableNonZeroExitCodes  Condition="'%(Lib.AcceptableNonZeroExitCodes)'   == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --     </Lib>
    --     <Midl>
    --         <TypeLibraryName              Condition="'%(Midl.TypeLibraryName)'                    == ''">$(IntDir)$(ProjectName).tlb</TypeLibraryName>
    --         <TargetEnvironment            Condition="'%(Midl.TargetEnvironment)'                  == ''">Win32</TargetEnvironment>
    --         <WarningLevel                 Condition="'%(Midl.WarningLevel)'                       == ''">1</WarningLevel>
    --         <DefaultCharType              Condition="'%(Midl.DefaultCharType)'                    == ''">Signed</DefaultCharType>
    --         <SuppressStartupBanner        Condition="'%(Midl.SuppressStartupBanner)'              == ''">true</SuppressStartupBanner>
    --         <TrackerLogDirectory          Condition="'%(Midl.TrackerLogDirectory)'                == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking   Condition="'%(Midl.MinimalRebuildFromTracking)'         == ''">true</MinimalRebuildFromTracking>
    --         <AcceptableNonZeroExitCodes   Condition="'%(Midl.AcceptableNonZeroExitCodes)'         == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --         <IgnoreStandardIncludePath    Condition="'%(Midl.IgnoreStandardIncludePath)'          == ''">false</IgnoreStandardIncludePath>
    --         <WarnAsError                  Condition="'%(Midl.WarnAsError)'                        == ''">false</WarnAsError>
    --         <GenerateTypeLibrary          Condition="'%(Midl.GenerateTypeLibrary)'                == ''">true</GenerateTypeLibrary>
    --         <ErrorCheckAllocations        Condition="'%(Midl.ErrorCheckAllocations)'              == ''">false</ErrorCheckAllocations>
    --         <ErrorCheckBounds             Condition="'%(Midl.ErrorCheckBounds)'                   == ''">false</ErrorCheckBounds>
    --         <ErrorCheckEnumRange          Condition="'%(Midl.ErrorCheckEnumRange)'                == ''">false</ErrorCheckEnumRange>
    --         <ErrorCheckRefPointers        Condition="'%(Midl.ErrorCheckRefPointers)'              == ''">false</ErrorCheckRefPointers>
    --         <ErrorCheckStubData           Condition="'%(Midl.ErrorCheckStubData)'                 == ''">false</ErrorCheckStubData>
    --         <StructMemberAlignment        Condition="'%(Midl.StructMemberAlignment)'              == ''">NotSet</StructMemberAlignment>
    --         <HeaderFileName               Condition="'%(Midl.HeaderFileName)'                     == ''">%(Filename)_h.h</HeaderFileName>
    --     </Midl>
    --     <ResourceCompile>
    --         <ResourceOutputFileName       Condition="'%(ResourceCompile.ResourceOutputFileName)'        == ''">$(IntDir)%(Filename).res</ResourceOutputFileName>
    --         <Culture                      Condition="'%(ResourceCompile.Culture)'                       == ''">0x0409</Culture>
    --         <TrackerLogDirectory          Condition="'%(ResourceCompile.TrackerLogDirectory)'           == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking   Condition="'%(ResourceCompile.MinimalRebuildFromTracking)'    == ''">true</MinimalRebuildFromTracking>
    --         <AcceptableNonZeroExitCodes   Condition="'%(ResourceCompile.AcceptableNonZeroExitCodes)'    == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --     </ResourceCompile>
    --     <Manifest>
    --         <TrackerLogDirectory          Condition="'%(Manifest.TrackerLogDirectory)'            == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking   Condition="'%(Manifest.MinimalRebuildFromTracking)'     == ''">true</MinimalRebuildFromTracking>
    --         <SuppressStartupBanner        Condition="'%(Manifest.SuppressStartupBanner)'          == ''">true</SuppressStartupBanner>
    --         <AcceptableNonZeroExitCodes   Condition="'%(Manifest.AcceptableNonZeroExitCodes)'     == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --         <VerboseOutput                Condition="'%(Manifest.VerboseOutput)'                  == ''">true</VerboseOutput>
    --         <GenerateCatalogFiles         Condition="'%(Manifest.GenerateCatalogFiles)'           == ''">false</GenerateCatalogFiles>
    --         <UpdateFileHashes             Condition="'%(Manifest.UpdateFileHashes)'               == ''">false</UpdateFileHashes>
    --         <OutputManifestFile           Condition="'$(EmbedManifest)'                           == 'false'" >$(TargetPath).manifest</OutputManifestFile>
    --         <OutputManifestFile           Condition="'$(EmbedManifest)'                           == 'true' AND '$(EmbedManifestBy)'=='LINK'">$(IntDir)$(TargetName)$(TargetExt).embed.manifest</OutputManifestFile>
    --     </Manifest>
    --     <ManifestResourceCompile>
    --         <ResourceOutputFileName       Condition="'%(ManifestResourceCompile.ResourceOutputFileName)' == ''">$(IntDir)$(TargetName)$(TargetExt).embed.manifest.res</ResourceOutputFileName>
    --     </ManifestResourceCompile>
    --     <XdcMake>
    --         <TrackerLogDirectory          Condition="'%(XdcMake.TrackerLogDirectory)'            == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking   Condition="'%(XdcMake.MinimalRebuildFromTracking)'     == ''">true</MinimalRebuildFromTracking>
    --         <SuppressStartupBanner        Condition="'%(XdcMake.SuppressStartupBanner)'          == ''">true</SuppressStartupBanner>
    --         <OutputFile                   Condition="'%(XdcMake.OutputFile)'                     == ''">$(OutDir)$(TargetName).xml</OutputFile>
    --         <AcceptableNonZeroExitCodes   Condition="'%(XdcMake.AcceptableNonZeroExitCodes)'     == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --         <UseUnicodeResponseFiles      Condition="'%(XdcMake.UseUnicodeResponseFiles)'        == ''">true</UseUnicodeResponseFiles>
    --         <ValidateIntelliSense         Condition="'%(XdcMake.ValidateIntelliSense)'           == ''">false</ValidateIntelliSense>
    --     </XdcMake>
    --     <BscMake>
    --         <TrackerLogDirectory          Condition="'%(BscMake.TrackerLogDirectory)'            == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking   Condition="'%(BscMake.MinimalRebuildFromTracking)'     == ''">true</MinimalRebuildFromTracking>
    --         <SuppressStartupBanner        Condition="'%(BscMake.SuppressStartupBanner)'          == ''">true</SuppressStartupBanner>
    --         <PreserveSBR                  Condition="'%(BscMake.PreserveSBR)'                    == ''">false</PreserveSBR>
    --         <OutputFile                   Condition="'%(BscMake.OutputFile)'                     == ''">$(OutDir)$(TargetName).bsc</OutputFile>
    --         <AcceptableNonZeroExitCodes   Condition="'%(BscMake.AcceptableNonZeroExitCodes)'     == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --     </BscMake>
    --     <XSD>
    --         <TrackerLogDirectory          Condition="'%(XSD.TrackerLogDirectory)'            == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking   Condition="'%(XSD.MinimalRebuildFromTracking)'     == ''">true</MinimalRebuildFromTracking>
    --         <SuppressStartupBanner        Condition="'%(XSD.SuppressStartupBanner)'          == ''">true</SuppressStartupBanner>
    --         <Language                     Condition="'%(XSD.Language)'                       == ''">cpp</Language>
    --         <Namespace                    Condition="'%(XSD.Namespace)'                      == ''">$(TargetName)</Namespace>
    --         <GenerateFromSchema           Condition="'%(XSD.GenerateFromSchema)'             == ''">dataset</GenerateFromSchema>
    --         <AcceptableNonZeroExitCodes   Condition="'%(XSD.AcceptableNonZeroExitCodes)'     == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --     </XSD>
    --     <CustomBuild>
    --         <TrackerLogDirectory          Condition="'%(CustomBuild.TrackerLogDirectory)'        == ''">$(IntDir)</TrackerLogDirectory>
    --         <MinimalRebuildFromTracking   Condition="'%(CustomBuild.MinimalRebuildFromTracking)' == ''">true</MinimalRebuildFromTracking>
    --         <AcceptableNonZeroExitCodes   Condition="'%(CustomBuild.AcceptableNonZeroExitCodes)' == ''">$(AcceptableNonZeroExitCodes)</AcceptableNonZeroExitCodes>
    --     </CustomBuild>
    --     <ProjectReference>
    --         <LinkLibraryDependencies Condition="'%(ProjectReference.LinkLibraryDependencies)' == '' and '$(ConfigurationType)' == 'StaticLibrary'">false</LinkLibraryDependencies>
    --         <LinkLibraryDependencies Condition="'%(ProjectReference.LinkLibraryDependencies)' == ''">true</LinkLibraryDependencies>
    --         <UseLibraryDependencyInputs Condition="'%(ProjectReference.UseLibraryDependencyInputs)' == ''">false</UseLibraryDependencyInputs>
    --         <ReferenceOutputAssembly Condition="'%(ProjectReference.ReferenceOutputAssembly)' == ''">true</ReferenceOutputAssembly>
    --         <Private Condition="'%(ProjectReference.Private)' == ''">true</Private>
    --     </ProjectReference>
    --     <Reference>
    --         <ReferenceOutputAssembly>true</ReferenceOutputAssembly>
    --         <CopyLocalSatelliteAssemblies>true</CopyLocalSatelliteAssemblies>
    --     </Reference>
    --     <COMReference>
    --         <ReferenceOutputAssembly>true</ReferenceOutputAssembly>
    --         <CopyLocalSatelliteAssemblies>true</CopyLocalSatelliteAssemblies>
    --     </COMReference>
    -- </ItemDefinitionGroup>

    -- <ItemGroup>
    --     -- <!--  Unique items for the project-->
    --     <CustomBuildStep Include="$(ProjectName)" />
    --     <PreBuildEvent Include="$(ProjectName)" />
    --     <PreLinkEvent Include="$(ProjectName)" />
    --     <PostBuildEvent Include="$(ProjectName)" />
    -- </ItemGroup>

    -- <!-- Import Platform specific settings -->
    (Platform !== "" &&& exists (VCTargetsPath \\ "Platforms" \\ Platform \\ "Microsoft.Cpp.$(Platform).props")) ? do
        include (VCTargetsPath \\ "Platforms" \\ Platform \\ "Microsoft.Cpp.$(Platform).props")
