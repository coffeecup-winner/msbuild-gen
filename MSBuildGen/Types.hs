{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module MSBuildGen.Types where

import Control.Monad.Free

type ProjectContext = Free ProjectContent ()
type PropertyGroupContext = Free PropertyGroupContent ()
type ItemDefinitionGroupContext = Free ItemDefinitionGroupContent ()
type ItemDefinitionContext = Free ItemDefinitionContent ()
type ItemGroupContext = Free ItemGroupContent ()
type TargetContext = Free TargetContent ()
type TaskContext = Free TaskContent ()
type SwitchContext = Free SwitchCase ()

data Project = Project String ProjectContext

data ProjectContent next = Import [MSBuildPath] next
                         | PropertyGroup PropertyGroupContext next
                         | ItemDefinitionGroup ItemDefinitionGroupContext next
                         | ItemGroup ItemGroupContext next
                         | UsingTask MSBuildTask String next
                         | TargetDefinition MSBuildTarget TargetContext next
                         | Cond MSBuildCondition ProjectContext next
                         deriving (Show, Functor)

data PropertyGroupContent next = PropertyAssignment MSBuildProperty MSBuildValue next
                               | PropertyCondition MSBuildCondition PropertyGroupContext next
                               deriving (Show, Functor)

data ItemDefinitionGroupContent next = ItemDefinition MSBuildItem ItemDefinitionContext next
                                     deriving (Show, Functor)

data ItemDefinitionContent next = MetadataAssignment MSBuildItemMetadata MSBuildValue next
                                | MetadataCondition MSBuildCondition ItemDefinitionContext next
                                deriving (Show, Functor)

data ItemGroupContent next = ItemInclude MSBuildItem MSBuildValue (Maybe ItemDefinitionContext) next
                           | ItemRemove MSBuildItem MSBuildValue next
                           | ItemCondition MSBuildCondition ItemGroupContext next
                           deriving (Show, Functor)

data TargetContent next = RunTask MSBuildTask TaskContext next
                        | TargetCondition MSBuildCondition TargetContext next
                        | TargetBeforeTargets MSBuildValue next
                        | TargetAfterTargets MSBuildValue next
                        | TargetDependsOn MSBuildValue next
                        | TargetInputs MSBuildValue next
                        | TargetOutputs MSBuildValue next
                        | TargetReturns MSBuildValue next
                        | TargetPropertyGroup PropertyGroupContext next
                        | TargetItemGroup ItemGroupContext next
                        deriving (Show, Functor)

data TaskContent next = TaskParameterAssignment MSBuildTaskParameter MSBuildValue next
                      deriving (Show, Functor)

data SwitchCase next = SwitchCase { caseKey :: MSBuildValue
                                  , caseValue :: MSBuildValue
                                  , caseNext :: next
                                  } deriving (Show, Functor)

data MSBuildProperty = Property String
                     deriving (Show)

data TString
data TList
data TBool
data TPath

type family TypeOf a

type instance TypeOf [Char] = TString
type instance TypeOf [[Char]] = TList
type instance TypeOf Bool = TBool
type instance TypeOf [MSBuildPath] = TPath

class Property a where
    toMSBuildProperty :: a -> MSBuildProperty

data MSBuildItem = Item String
                 deriving (Show)

class Item a where
    toMSBuildItem :: a -> MSBuildItem

data MSBuildItemMetadata = ItemMetadata String
                         deriving (Show)

class ItemMetadata a where
    toMSBuildItemMetadata :: a -> MSBuildItemMetadata

data MSBuildPath = StringPath String
                 | PropertyPath MSBuildProperty
                 | MetadataPath MSBuildItemMetadata
                 | SeparatorPath
                 deriving (Show)

class Path a where
    toMSBuildPath :: a -> [MSBuildPath]

instance Path [Char] where
    toMSBuildPath s = [StringPath s]

instance Path MSBuildProperty where
    toMSBuildPath p = [PropertyPath p]

instance Path MSBuildItemMetadata where
    toMSBuildPath m = [MetadataPath m]

instance Path [MSBuildPath] where
    toMSBuildPath = id

data MSBuildValue = StringValue String
                  | ListValue [String]
                  | PathValue [MSBuildPath]
                  | BoolValue Bool
                  | PropertyValue MSBuildProperty
                  | ItemValue MSBuildItem
                  deriving (Show)

class Value a where
    toMSBuildValue :: a -> MSBuildValue

instance Value [Char] where
    toMSBuildValue = StringValue

instance Value [[Char]] where
    toMSBuildValue = ListValue

instance Value [MSBuildPath] where
    toMSBuildValue = PathValue

instance Value Bool where
    toMSBuildValue = BoolValue

instance Value MSBuildProperty where
    toMSBuildValue = PropertyValue

instance Value MSBuildValue where
    toMSBuildValue = id

data MSBuildCondition = Leaf String
                      | PropertyRef MSBuildProperty
                      | ItemRef MSBuildItem
                      | MetadataRef MSBuildItemMetadata
                      | ValueRef MSBuildValue
                      | Or MSBuildCondition MSBuildCondition
                      | And MSBuildCondition MSBuildCondition
                      | Equal MSBuildCondition MSBuildCondition
                      | NotEqual MSBuildCondition MSBuildCondition
                      | Exists MSBuildValue
                      deriving (Show)

class Condition a where
    toMSBuildCondition :: a -> MSBuildCondition

instance Condition [Char] where
    toMSBuildCondition = Leaf

instance Condition Bool where
    toMSBuildCondition True = Leaf "true"
    toMSBuildCondition False = Leaf "false"

instance Condition MSBuildProperty where
    toMSBuildCondition = PropertyRef

instance Condition MSBuildItem where
    toMSBuildCondition = ItemRef

instance Condition MSBuildItemMetadata where
    toMSBuildCondition = MetadataRef

instance Condition MSBuildValue where
    toMSBuildCondition = ValueRef

instance Condition MSBuildCondition where
    toMSBuildCondition = id

data MSBuildTarget = Target String
                   deriving (Show)

class Target a where
    toMSBuildTarget :: a -> MSBuildTarget

instance Target MSBuildTarget where
    toMSBuildTarget = id

data MSBuildTask = Task String
                 deriving (Show)

class Task a where
    toMSBuildTask :: a -> MSBuildTask

instance Task MSBuildTask where
    toMSBuildTask = id

data MSBuildTaskParameter = TaskParameter String
                          deriving (Show)

class TaskParameter a where
    toMSBuildTaskParameter :: a -> MSBuildTaskParameter

instance TaskParameter MSBuildTaskParameter where
    toMSBuildTaskParameter = id
