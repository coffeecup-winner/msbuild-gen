module MSBuildGen.DSL.Core where

import Control.Monad
import Language.Haskell.TH

simpleData :: String -> DecsQ -> DecsQ
simpleData n = liftM2 (:) (dataD (cxt []) name [] [normalC name []] [])
    where name = mkName n
