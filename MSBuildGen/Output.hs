{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MSBuildGen.Output where

import Data.List (intercalate)

import Text.XML.Light

import MSBuildGen.Types

writeProject :: Project -> IO ()
writeProject p@(Project n _) = writeFile n $ ppTopElement $ genProject p

genProject :: Project -> Element
genProject (Project _ p) = Element (qname "Project") [attr "xmlns" "http://schemas.microsoft.com/developer/msbuild/2003"] (genContent p) Nothing

genContent :: ProjectContext -> [Content]
genContent = imap $ \case
    Import s _ -> return $ element "Import" [attr "Project" $ render () s] []
    PropertyGroup p _ -> return $ element "PropertyGroup" [] (genPropertyGroup p)
    ItemDefinitionGroup g _ -> return $ element "ItemDefinitionGroup" [] (genItemDefinitionGroup g)
    ItemGroup g _ -> return $ element "ItemGroup" [] (genItemGroup g)
    UsingTask t s _ -> return $ element "UsingTask" [attr "TaskName" $ render () t, attr "AssemblyName" s] []
    TargetDefinition n t _ -> return $ element "Target" (attr "Name" (render () n) : genTargetAttrs t) (genTarget t)
    Cond c p _ -> genContent' c p

genContent' :: MSBuildCondition -> ProjectContext -> [Content]
genContent' c = imap $ \case
    Import s _ -> return $ element "Import" [attr "Project" $ render () s, attr "Condition" $ render () c] []
    PropertyGroup p _ -> return $ element "PropertyGroup" [attr "Condition" $ render () c] (genPropertyGroup p)
    ItemGroup g _ -> return $ element "ItemGroup" [attr "Condition" $ render () c] (genItemGroup g)
    TargetDefinition n t _ -> return $ element "Target" (attr "Name" (render () n) : attr "Condition" (render () c) : genTargetAttrs t) (genTarget t)

genPropertyGroup :: PropertyGroupContext -> [Content]
genPropertyGroup = imap $ \case
    PropertyAssignment p v _ -> return $ element (render () p) [] [text $ render () v]
    PropertyCondition c p _ -> genPropertyGroup' c p

genPropertyGroup' :: MSBuildCondition -> PropertyGroupContext -> [Content]
genPropertyGroup' c = imap $ \case
    PropertyAssignment p v _ -> return $ element (render () p) [attr "Condition" $ render () c] [text $ render () v]

genItemDefinitionGroup :: ItemDefinitionGroupContext -> [Content]
genItemDefinitionGroup = imap $ \case
    ItemDefinition i ms _ -> return $ element (render () i) [] (genItemDefinition i ms)

genItemDefinition :: MSBuildItem -> ItemDefinitionContext -> [Content]
genItemDefinition i = imap $ \case
    MetadataAssignment m v _ -> return $ element (render () m) [] [text $ render () v]
    MetadataCondition c m _ -> genItemDefinition' i c m

genItemDefinition' :: MSBuildItem -> MSBuildCondition -> ItemDefinitionContext -> [Content]
genItemDefinition' i c = imap $ \case
    MetadataAssignment m v _ -> return $ element (render () m) [attr "Condition" $ render i c] [text $ render () v]

genItemGroup :: ItemGroupContext -> [Content]
genItemGroup = imap $ \case
    ItemInclude i v Nothing _ -> return $ element (render () i) [attr "Include" $ render () v] []
    ItemInclude i v (Just ms) _ -> return $ element (render () i) [attr "Include" $ render () v] (genItemDefinition i ms)
    ItemRemove i v _ -> return $ element (render () i) [attr "Remove" $ render () v] []
    ItemCondition c i _ -> genItemGroup' c i

genItemGroup' :: MSBuildCondition -> ItemGroupContext -> [Content]
genItemGroup' c = imap $ \case
    ItemInclude i (ItemValue v) Nothing _ -> return $ element (render () i) [attr "Include" $ render () v, attr "Condition" $ render v c] []
    ItemInclude i (PropertyValue v) Nothing _ -> return $ element (render () i) [attr "Include" $ render () v, attr "Condition" $ render () c] []
    ItemInclude i (ItemValue v) (Just ms) _ -> return $ element (render () i) [attr "Include" $ render () v, attr "Condition" $ render v c] (genItemDefinition i ms)
    ItemInclude i (PropertyValue v) (Just ms) _ -> return $ element (render () i) [attr "Include" $ render () v, attr "Condition" $ render () c] (genItemDefinition i ms)
    ItemRemove i (ItemValue v) _ -> return $ element (render () i) [attr "Remove" $ render () v, attr "Condition" $ render v c] []
    ItemRemove i (PropertyValue v) _ -> return $ element (render () i) [attr "Remove" $ render () v, attr "Condition" $ render () c] []

genTargetAttrs :: TargetContext -> [Attr]
genTargetAttrs = imap $ \case
    RunTask _ _ _ -> []
    TargetCondition _ _ _ -> []
    TargetBeforeTargets v _ -> return $ attr "BeforeTargets" (render () v)
    TargetAfterTargets v _ -> return $ attr "AfterTargets" (render () v)
    TargetDependsOn v _ -> return $ attr "DependsOnTargets" (render () v)
    TargetInputs v _ -> return $ attr "Inputs" (render () v)
    TargetOutputs v _ -> return $ attr "Outputs" (render () v)
    TargetReturns v _ -> return $ attr "Returns" (render () v)
    TargetPropertyGroup _ _ -> []
    TargetItemGroup _ _ -> []

genTarget :: TargetContext -> [Content]
genTarget = imap $ \case
    RunTask n t _ -> return $ element (render () n) (genTask t) []
    TargetCondition c t _ -> genTarget' c t
    TargetBeforeTargets _ _ -> []
    TargetAfterTargets _ _ -> []
    TargetDependsOn _ _ -> []
    TargetInputs _ _ -> []
    TargetOutputs _ _ -> []
    TargetReturns _ _ -> []
    TargetPropertyGroup g _ -> return $ element "PropertyGroup" [] (genPropertyGroup g)
    TargetItemGroup g _ -> return $ element "ItemGroup" [] (genItemGroup g)

genTarget' :: MSBuildCondition -> TargetContext -> [Content]
genTarget' c = imap $ \case
    RunTask n t _ -> return $ element (render () n) ((attr "Condition" $ render () c) : genTask t) []
    TargetPropertyGroup g _ -> return $ element "PropertyGroup" [attr "Condition" $ render () c] (genPropertyGroup g)
    TargetItemGroup g _ -> return $ element "ItemGroup" [attr "Condition" $ render () c] (genItemGroup g)

genTask :: TaskContext -> [Attr]
genTask = imap $ \case
    TaskParameterAssignment p v _ -> return $ attr (render () p) (render () v)

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
