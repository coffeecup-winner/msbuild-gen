module Main where

import MSBuildGen

import Examples.MicrosoftBuildStepsTargets
import Examples.MicrosoftCppProps
import Examples.MicrosoftCppDefaultProps
import Examples.MicrosoftCppInvalidPlatformTargets

main :: IO ()
main = mapM_ writeProject [ microsoft_build_steps_targets
                          , microsoft_cpp_props
                          , microsoft_cpp_default_props
                          , microsoft_cpp_invalid_platform_targets
                          ]
