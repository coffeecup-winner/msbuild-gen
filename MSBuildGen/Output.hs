{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MSBuildGen.Output where

import Control.Monad.Free
import Data.List (intercalate)

import Text.XML.Light

import MSBuildGen.Types

writeProject :: Project -> IO ()
writeProject p@(Project n _) = writeFile n $ ppTopElement $ genProject p

genProject :: Project -> Element
genProject (Project _ p) = Element (qname "Project") [attr "xmlns" "http://schemas.microsoft.com/developer/msbuild/2003"] (genContent p) Nothing

genContent :: ProjectContext -> [Content]
genContent (Free (Import s next)) = element "Import" [attr "Project" $ render () s] [] : genContent next
genContent (Free (PropertyGroup p next)) = element "PropertyGroup" [] (genPropertyGroup p) : genContent next
genContent (Free (ItemDefinitionGroup g next)) = element "ItemDefinitionGroup" [] (genItemDefinitionGroup g) : genContent next
genContent (Free (ItemGroup g next)) = element "ItemGroup" [] (genItemGroup g) : genContent next
genContent (Free (UsingTask t s next)) = element "UsingTask" [attr "TaskName" $ render () t, attr "AssemblyName" s] [] : genContent next
genContent (Free (TargetDefinition n t next)) = element "Target" (attr "Name" (render () n) : genTargetAttrs t) (genTarget t) : genContent next
genContent (Free (Cond c p next)) = genContent' c p ++ genContent next
genContent (Pure _) = []

genContent' :: MSBuildCondition -> ProjectContext -> [Content]
genContent' c (Free (Import s next)) = element "Import" [attr "Project" $ render () s, attr "Condition" $ render () c] [] : genContent next
genContent' c (Free (PropertyGroup p next)) = element "PropertyGroup" [attr "Condition" $ render () c] (genPropertyGroup p) : genContent next
genContent' c (Free (ItemGroup g next)) = element "ItemGroup" [attr "Condition" $ render () c] (genItemGroup g) : genContent next
genContent' c (Free (TargetDefinition n t next)) = element "Target" (attr "Name" (render () n) : attr "Condition" (render () c) : genTargetAttrs t) (genTarget t) : genContent next
genContent' _ (Pure _) = []

genPropertyGroup :: PropertyGroupContext -> [Content]
genPropertyGroup (Free (PropertyAssignment p v next)) = element (render () p) [] [text $ render () v] : genPropertyGroup next
genPropertyGroup (Free (PropertyCondition c p next)) = genPropertyGroup' c p ++ genPropertyGroup next
genPropertyGroup (Pure _) = []

genPropertyGroup' :: MSBuildCondition -> PropertyGroupContext -> [Content]
genPropertyGroup' c (Free (PropertyAssignment p v next)) = element (render () p) [attr "Condition" $ render () c] [text $ render () v] : genPropertyGroup next
genPropertyGroup' _ (Pure _) = []

genItemDefinitionGroup :: ItemDefinitionGroupContext -> [Content]
genItemDefinitionGroup (Free (ItemDefinition i ms next)) = element (render () i) [] (genItemDefinition i ms) : genItemDefinitionGroup next
genItemDefinitionGroup (Pure _) = []

genItemDefinition :: MSBuildItem -> ItemDefinitionContext -> [Content]
genItemDefinition i (Free (MetadataAssignment m v next)) = element (render () m) [] [text $ render () v] : genItemDefinition i next
genItemDefinition i (Free (MetadataCondition c m next)) = genItemDefinition' i c m ++ genItemDefinition i next
genItemDefinition _ (Pure _) = []

genItemDefinition' :: MSBuildItem -> MSBuildCondition -> ItemDefinitionContext -> [Content]
genItemDefinition' i c (Free (MetadataAssignment m v next)) = element (render () m) [attr "Condition" $ render i c] [text $ render () v] : genItemDefinition i next

genItemGroup :: ItemGroupContext -> [Content]
genItemGroup (Free (ItemInclude i v Nothing next)) = element (render () i) [attr "Include" $ render () v] [] : genItemGroup next
genItemGroup (Free (ItemInclude i v (Just ms) next)) = element (render () i) [attr "Include" $ render () v] (genItemDefinition i ms) : genItemGroup next
genItemGroup (Free (ItemRemove i v next)) = element (render () i) [attr "Remove" $ render () v] [] : genItemGroup next
genItemGroup (Free (ItemCondition c i next)) = genItemGroup' c i ++ genItemGroup next
genItemGroup (Pure _) = []

genItemGroup' :: MSBuildCondition -> ItemGroupContext -> [Content]
genItemGroup' c (Free (ItemInclude i (ItemValue v) Nothing next)) = element (render () i) [attr "Include" $ render () v, attr "Condition" $ render v c] [] : genItemGroup next
genItemGroup' c (Free (ItemInclude i (PropertyValue v) Nothing next)) = element (render () i) [attr "Include" $ render () v, attr "Condition" $ render () c] [] : genItemGroup next
genItemGroup' c (Free (ItemInclude i (ItemValue v) (Just ms) next)) = element (render () i) [attr "Include" $ render () v, attr "Condition" $ render v c] (genItemDefinition i ms) : genItemGroup next
genItemGroup' c (Free (ItemInclude i (PropertyValue v) (Just ms) next)) = element (render () i) [attr "Include" $ render () v, attr "Condition" $ render () c] (genItemDefinition i ms) : genItemGroup next
genItemGroup' c (Free (ItemRemove i (ItemValue v) next)) = element (render () i) [attr "Remove" $ render () v, attr "Condition" $ render v c] [] : genItemGroup next
genItemGroup' c (Free (ItemRemove i (PropertyValue v) next)) = element (render () i) [attr "Remove" $ render () v, attr "Condition" $ render () c] [] : genItemGroup next
genItemGroup' _ (Pure _) = []

genTargetAttrs :: TargetContext -> [Attr]
genTargetAttrs (Free (RunTask _ _ next)) = genTargetAttrs next
genTargetAttrs (Free (TargetCondition _ _ next)) = genTargetAttrs next
genTargetAttrs (Free (TargetBeforeTargets v next)) = attr "BeforeTargets" (render () v) : genTargetAttrs next
genTargetAttrs (Free (TargetAfterTargets v next)) = attr "AfterTargets" (render () v) : genTargetAttrs next
genTargetAttrs (Free (TargetDependsOn v next)) = attr "DependsOnTargets" (render () v) : genTargetAttrs next
genTargetAttrs (Free (TargetInputs v next)) = attr "Inputs" (render () v) : genTargetAttrs next
genTargetAttrs (Free (TargetOutputs v next)) = attr "Outputs" (render () v) : genTargetAttrs next
genTargetAttrs (Free (TargetReturns v next)) = attr "Returns" (render () v) : genTargetAttrs next
genTargetAttrs (Free (TargetPropertyGroup _ next)) = genTargetAttrs next
genTargetAttrs (Free (TargetItemGroup _ next)) = genTargetAttrs next
genTargetAttrs (Pure _) = []

genTarget :: TargetContext -> [Content]
genTarget (Free (RunTask n t next)) = element (render () n) (genTask t) [] : genTarget next
genTarget (Free (TargetCondition c t next)) = genTarget' c t ++ genTarget next
genTarget (Free (TargetBeforeTargets _ next)) = genTarget next
genTarget (Free (TargetAfterTargets _ next)) = genTarget next
genTarget (Free (TargetDependsOn _ next)) = genTarget next
genTarget (Free (TargetInputs _ next)) = genTarget next
genTarget (Free (TargetOutputs _ next)) = genTarget next
genTarget (Free (TargetReturns _ next)) = genTarget next
genTarget (Free (TargetPropertyGroup g next)) = element "PropertyGroup" [] (genPropertyGroup g) : genTarget next
genTarget (Free (TargetItemGroup g next)) = element "ItemGroup" [] (genItemGroup g) : genTarget next
genTarget (Pure _) = []

genTarget' :: MSBuildCondition -> TargetContext -> [Content]
genTarget' c (Free (RunTask n t next)) = element (render () n) ((attr "Condition" $ render () c) : genTask t) [] : genTarget next
genTarget' c (Free (TargetPropertyGroup g next)) = element "PropertyGroup" [attr "Condition" $ render () c] (genPropertyGroup g) : genTarget next
genTarget' c (Free (TargetItemGroup g next)) = element "ItemGroup" [attr "Condition" $ render () c] (genItemGroup g) : genTarget next
genTarget' _ (Pure _) = []

genTask :: TaskContext -> [Attr]
genTask (Free (TaskParameterAssignment p v next)) = attr (render () p) (render () v) : genTask next
genTask (Pure _) = []

class Render a b where
    render :: a -> b -> String

instance Render () MSBuildProperty where
    render () (Property s) = s

instance Render () MSBuildItem where
    render () (Item s) = s

instance Render () MSBuildItemMetadata where
    render () (ItemMetadata s) = s

instance Render () MSBuildPath where
    render () (StringPath s) = s
    render () (PropertyPath (Property n)) = "$(" ++ n ++ ")"
    render () (MetadataPath (ItemMetadata n)) = "%(" ++ n ++ ")"
    render () (SeparatorPath) = "\\"

instance Render () [MSBuildPath] where
    render () ps = concatMap (render ()) ps

instance Render () MSBuildValue where
    render () (StringValue s) = s
    render () (ListValue ss) = intercalate ";" ss
    render () (PathValue ps) = render () ps
    render () (BoolValue True) = "true"
    render () (BoolValue False) = "false"
    render () (PropertyValue (Property n)) = "$(" ++ n ++ ")"
    render () (ItemValue (Item n)) = "@(" ++ n ++ ")"

instance Render () MSBuildCondition where
    render () (Leaf s) = s
    render () (PropertyRef (Property n)) = "$(" ++ n ++ ")"
    render () (ItemRef (Item n)) = "@(" ++ n ++ ")"
--    render () (MetadataRef (ItemMetadata _ n)) = "%(" ++ n ++ ")"
    render () (ValueRef v) = render () v
    render () (Or a b) = "(" ++ render () a ++ " or " ++ render () b ++ ")"
    render () (And a b) = render () a ++ " and " ++ render () b
    render () (Equal a b) = "'" ++ render () a ++ "' == '" ++ render () b ++ "'"
    render () (NotEqual a b) = "'" ++ render () a ++ "' != '" ++ render () b ++ "'"
    render () (Exists s) = "exists('" ++ render () s ++ "')"

instance Render () MSBuildTarget where
    render () (Target t) = t

instance Render () MSBuildTask where
    render () (Task t) = t

instance Render () MSBuildTaskParameter where
    render () (TaskParameter p) = p

instance Render MSBuildItem MSBuildCondition where
    render _ (Leaf s) = s
    render _ (PropertyRef (Property n)) = "$(" ++ n ++ ")"
    render _ (ItemRef (Item n)) = "@(" ++ n ++ ")"
    render i (MetadataRef (ItemMetadata n)) = "%(" ++ render () i ++ "." ++ n ++ ")"
    render _ (ValueRef v) = render () v
    render i (Or a b) = "(" ++ render i a ++ " or " ++ render i b ++ ")"
    render i (And a b) = render i a ++ " and " ++ render i b
    render i (Equal a b) = "'" ++ render i a ++ "' == '" ++ render i b ++ "'"
    render i (NotEqual a b) = "'" ++ render i a ++ "' != '" ++ render i b ++ "'"
    render _ (Exists s) = "exists('" ++ render () s ++ "')"

qname :: String -> QName
qname s = QName s Nothing Nothing

attr :: String -> String -> Attr
attr k = Attr $ qname k

element :: String -> [Attr] -> [Content] -> Content
element s as cs = Elem $ Element (qname s) as cs Nothing

text :: String -> Content
text s = Text $ CData CDataText s Nothing
