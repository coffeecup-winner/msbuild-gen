module MSBuildGen.Syntax where

import Control.Monad.Free

import MSBuildGen.Types

class Functor a => Conditionable a where
    condition :: MSBuildCondition -> Free a () -> () -> a ()

instance Conditionable ProjectContent where
    condition = Cond

instance Conditionable PropertyGroupContent where
    condition = PropertyCondition

project :: String -> ProjectContext -> Project
project = Project

propertyGroup :: PropertyGroupContext -> ProjectContext
propertyGroup p = liftF $ PropertyGroup p ()

(=:) :: (Property p, Value v) => p -> v -> PropertyGroupContext
p =: v = liftF $ Assignment (toMSBuildProperty p) (toMSBuildValue v) ()

(=?) :: (Property p, Value v) => p -> v -> PropertyGroupContext
p =? v = (toMSBuildProperty p === "") ? (p =: v)

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

