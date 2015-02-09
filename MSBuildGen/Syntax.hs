{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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

class Functor ctx => Assignable lhs rhs ctx where
    (=:) :: lhs -> rhs -> Free ctx ()

instance (Property p, Value v) => Assignable p v PropertyGroupContent where
    (=:) p v = liftF $ PropertyAssignment (toMSBuildProperty p) (toMSBuildValue v) ()

instance (ItemMetadata m, Value v) => Assignable m v ItemDefinitionContent where
    (=:) m v = liftF $ MetadataAssignment (toMSBuildItemMetadata m) (toMSBuildValue v) ()

project :: String -> ProjectContext -> Project
project = Project

propertyGroup :: PropertyGroupContext -> ProjectContext
propertyGroup p = liftF $ PropertyGroup p ()

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

itemGroup :: ProjectContext
itemGroup = liftF $ ItemGroup ()

include :: Value v => v -> ProjectContext
include v = liftF $ Import (toMSBuildValue v) ()

target :: ProjectContext
target = liftF $ Target ()

exists :: Value v => v -> MSBuildCondition
exists = Exists . toMSBuildValue

(?) :: (Condition c, Conditionable a) => c -> Free a () -> Free a ()
c ? p = liftF $ condition (toMSBuildCondition c) p ()

infixr 2 |||
infixr 3 &&&
infix 4 ===
infix 4 !==
infix 8 =:
infix 9 ?

switch :: (Property p) => p -> String -> SwitchContext -> PropertyGroupContext
switch p n s = go s
    where go (Free (SwitchCase k v next)) = (n === k) ? (p =: v) >> go next
          go (Pure _) = Pure ()

(-->) :: (Value k, Value v) => k -> v -> SwitchContext
k --> v = liftF $ SwitchCase (toMSBuildValue k) (toMSBuildValue v) ()

(\\) :: (Value a, Value b) => a -> b -> MSBuildValue
a \\ b = go (toMSBuildValue a) (toMSBuildValue b)
    where go (PathValue as) (PathValue bs) = PathValue (as ++ bs)
          go (PathValue as) vb = PathValue (as ++ [vb])
          go va (PathValue bs) = PathValue (va : bs)
          go va vb = PathValue [va, vb]

