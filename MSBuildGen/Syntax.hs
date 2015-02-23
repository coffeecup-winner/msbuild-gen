{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
module MSBuildGen.Syntax where

import Control.Monad.Free

import MSBuildGen.Types

class Functor ctx => Conditionable ctx where
    condition :: MSBuildCondition -> Free ctx () -> () -> ctx ()

instance Conditionable ProjectContent where
    condition = Cond

instance Conditionable PropertyGroupContent where
    condition = PropertyCondition

instance Conditionable ItemDefinitionContent where
    condition = MetadataCondition

instance Conditionable ItemGroupContent where
    condition = ItemCondition

instance Conditionable TargetContent where
    condition = TargetCondition

type family CanAssignFrom a b where
    CanAssignFrom TString TString = True
    CanAssignFrom TString TPath = True
    CanAssignFrom TList TList = True
    CanAssignFrom TBool TBool = True
    CanAssignFrom TPath TPath = True
    CanAssignFrom TPath TString = True

type CanAssign a b = (CanAssignFrom (TypeOf a) (TypeOf b) ~ True)

class Functor ctx => Assignable lhs rhs ctx where
    (=:) :: lhs -> rhs -> Free ctx ()

instance (Property p, Value v, CanAssign p v) => Assignable p v PropertyGroupContent where
    p =: v = liftF $ PropertyAssignment (toMSBuildProperty p) (toMSBuildValue v) ()

instance (ItemMetadata m, Value v, CanAssign m v) => Assignable m v ItemDefinitionContent where
    m =: v = liftF $ MetadataAssignment (toMSBuildItemMetadata m) (toMSBuildValue v) ()

instance (TaskParameter p, Value v, CanAssign p v) => Assignable p v TaskContent where
    p =: v = liftF $ TaskParameterAssignment (toMSBuildTaskParameter p) (toMSBuildValue v) ()

class Functor ctx => DefinitionContext ctx where
    propertyGroup :: PropertyGroupContext -> Free ctx ()
    itemGroup :: ItemGroupContext -> Free ctx ()

instance DefinitionContext ProjectContent where
    propertyGroup g = liftF $ PropertyGroup g ()
    itemGroup g = liftF $ ItemGroup g ()

instance DefinitionContext TargetContent where
    propertyGroup g = liftF $ TargetPropertyGroup g ()
    itemGroup g = liftF $ TargetItemGroup g ()

project :: String -> ProjectContext -> Project
project = Project

(=?) :: (Assignable lhs rhs ctx, Condition lhs, Conditionable ctx) => lhs -> rhs -> Free ctx ()
lhs =? rhs = (lhs === "") ? (lhs =: rhs)

itemDefinitionGroup :: ItemDefinitionGroupContext -> ProjectContext
itemDefinitionGroup g = liftF $ ItemDefinitionGroup g ()

item :: (Item i) => i -> ItemDefinitionContext -> ItemDefinitionGroupContext
item i c = liftF $ ItemDefinition (toMSBuildItem i) c ()

(|||) :: (Condition a, Condition b) => a -> b -> MSBuildCondition
a ||| b = Or (toMSBuildCondition a) (toMSBuildCondition b)

(&&&) :: (Condition a, Condition b) => a -> b -> MSBuildCondition
a &&& b = And (toMSBuildCondition a) (toMSBuildCondition b)

(===) :: (Condition a, Condition b) => a -> b -> MSBuildCondition
a === b = Equal (toMSBuildCondition a) (toMSBuildCondition b)

(!==) :: (Condition a, Condition b) => a -> b -> MSBuildCondition
a !== b = NotEqual (toMSBuildCondition a) (toMSBuildCondition b)

(<:) :: (Item i, Value v) => i -> v -> ItemGroupContext
i <: v = liftF $ ItemInclude (toMSBuildItem i) (toMSBuildValue v) Nothing ()

(<:!) :: (Item i, Value v) => i -> v -> ItemDefinitionContext -> ItemGroupContext
i <:! v = \ctx -> liftF $ ItemInclude (toMSBuildItem i) (toMSBuildValue v) (Just ctx) ()

(</:) :: (Item i, Value v) => i -> v -> ItemGroupContext
i </: v = liftF $ ItemRemove (toMSBuildItem i) (toMSBuildValue v) ()

include :: Path p => p -> ProjectContext
include p = liftF $ Import (toMSBuildPath p) ()

exists :: Value v => v -> MSBuildCondition
exists = Exists . toMSBuildValue

(?) :: (Condition c, Conditionable a) => c -> Free a () -> Free a ()
c ? p = liftF $ condition (toMSBuildCondition c) p ()

infixr 2 |||
infixr 3 &&&
infix 4 ===
infix 4 !==
infix 8 =:
infix 8 =?
infix 8 <:
infix 8 <:!
infix 8 </:
infix 9 ?
infixr 3 <>
infixr 3 \\

(\\) :: (Path a, Path b) => a -> b -> [MSBuildPath]
a \\ b = (toMSBuildPath a) ++ [SeparatorPath] ++ (toMSBuildPath b)

(<>) :: (Path a, Path b) => a -> b -> [MSBuildPath]
a <> b = (toMSBuildPath a) ++ (toMSBuildPath b)

using :: Task t => t -> String -> ProjectContext
using t s = liftF $ UsingTask (toMSBuildTask t) s ()

target :: Target t => t -> TargetContext -> ProjectContext
target t c = liftF $ TargetDefinition (toMSBuildTarget t) c ()

before :: Value v => v -> TargetContext
before v = liftF $ TargetBeforeTargets (toMSBuildValue v) ()

after :: Value v => v -> TargetContext
after v = liftF $ TargetAfterTargets (toMSBuildValue v) ()

dependsOn :: Value v => v -> TargetContext
dependsOn v = liftF $ TargetDependsOn (toMSBuildValue v) ()

inputs :: Value v => v -> TargetContext
inputs v = liftF $ TargetInputs (toMSBuildValue v) ()

outputs :: Value v => v -> TargetContext
outputs v = liftF $ TargetOutputs (toMSBuildValue v) ()

returns :: Value v => v -> TargetContext
returns v = liftF $ TargetReturns (toMSBuildValue v) ()

run :: Task t => t -> TaskContext -> TargetContext
run t c = liftF $ RunTask (toMSBuildTask t) c ()
