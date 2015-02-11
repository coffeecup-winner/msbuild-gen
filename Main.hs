module Main where

import MSBuildGen

import Examples.MicrosoftCppProps
import Examples.MicrosoftCppDefaultProps
import Examples.MicrosoftCppInvalidPlatformTargets

main :: IO ()
main = mapM_ writeProject [ microsoft_cpp_props
                          , microsoft_cpp_default_props
                          , microsoft_cpp_invalid_platform_targets
                          ]
