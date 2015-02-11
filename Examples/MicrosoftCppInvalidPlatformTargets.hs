module Examples.MicrosoftCppInvalidPlatformTargets where

import MSBuildGen
import Examples.KnownProperties
import Examples.KnownItems
import Examples.KnownTargets

microsoft_cpp_invalid_platform_targets :: Project
microsoft_cpp_invalid_platform_targets = project "Microsoft.Cpp.InvalidPlatform.targets" $ do -- InitialTargets="InvalidPlatformError"
    -- <!--
    -- ***********************************************************************************************
    -- Microsoft.Cpp.InvalidPlatform.targets

    -- WARNING:  DO NOT MODIFY this file unless you are knowledgeable about MSBuild and have
    --           created a backup copy.  Incorrect changes to this file will make it
    --           impossible to load or build your projects from the command-line or the IDE.

    -- This file defines the information needed to deal with the case where an invalid platform is
    -- passed to the project, so that errors occur at build- instead of load-time.

    -- Copyright (C) Microsoft Corporation. All rights reserved.
    -- ***********************************************************************************************
    -- -->
    using VCMessage "Microsoft.Build.CppTasks.Common, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"

    target InvalidPlatformError $ do
        (BuildingInsideVisualStudio === True) ? do
            run VCMessage $ do
                Code =: "MSB8006"
                Type =: "Error"
                Arguments =: "$(MSBuildProjectFile);$(Platform)"
        (BuildingInsideVisualStudio !== True) ? do
            run VCMessage $ do
                Code =: "MSB8007"
                Type =: "Error"
                Arguments =: "$(MSBuildProjectFile);$(Platform)"
