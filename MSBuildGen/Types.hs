{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
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

data ProjectContent next = Import MSBuildValue next
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

data ItemGroupContent next = ItemInclude MSBuildItem MSBuildValue next
                           deriving (Show, Functor)

data TargetContent next = RunTask MSBuildTask TaskContext next
                        | TargetCondition MSBuildCondition TargetContext next
                        deriving (Show, Functor)

data TaskContent next = TaskParameterAssignment MSBuildTaskParameter MSBuildValue next
                      deriving (Show, Functor)

data SwitchCase next = SwitchCase { caseKey :: MSBuildValue
                                  , caseValue :: MSBuildValue
                                  , caseNext :: next
                                  } deriving (Show, Functor)

data Type = String
          | List
          | Path
          | Bool
          deriving (Show)

data MSBuildProperty = Property Type String
                     deriving (Show)

class Property a where
    toMSBuildProperty :: a -> MSBuildProperty

data MSBuildItem = Item String
                 deriving (Show)

class Item a where
    toMSBuildItem :: a -> MSBuildItem

data MSBuildItemMetadata = ItemMetadata Type String
                         deriving (Show)

class ItemMetadata a where
    toMSBuildItemMetadata :: a -> MSBuildItemMetadata

data MSBuildValue = StringValue String
                  | ListValue [String]
                  | PathValue [MSBuildValue]
                  | BoolValue Bool
                  | PropertyValue MSBuildProperty
                  deriving (Show)

class Value a where
    toMSBuildValue :: a -> MSBuildValue

instance Value [Char] where
    toMSBuildValue = StringValue

instance Value [[Char]] where
    toMSBuildValue = ListValue

instance Value Bool where
    toMSBuildValue = BoolValue

instance Value MSBuildProperty where
    toMSBuildValue = PropertyValue

instance Value MSBuildValue where
    toMSBuildValue = id

data MSBuildCondition = Leaf String
                    | PropertyRef MSBuildProperty
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

data MSBuildTaskParameter = TaskParameter Type String
                          deriving (Show)

class TaskParameter a where
    toMSBuildTaskParameter :: a -> MSBuildTaskParameter

instance TaskParameter MSBuildTaskParameter where
    toMSBuildTaskParameter = id
