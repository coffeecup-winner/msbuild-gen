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
genContent (Free (Import s next)) = element "Import" [attr "Project" $ render s] [] : genContent next
genContent (Free (PropertyGroup p next)) = element "PropertyGroup" [] (genPropertyGroup p) : genContent next
genContent (Free (ItemGroup next)) = genContent next
genContent (Free (Target next)) = genContent next
genContent (Free (Cond c p next)) = genContent' c p ++ genContent next
genContent (Pure _) = []

genContent' :: MSBuildCondition -> ProjectContext -> [Content]
genContent' c (Free (Import s next)) = element "Import" [attr "Project" $ render s, attr "Condition" $ render c] [] : genContent next
genContent' c (Free (PropertyGroup p next)) = element "PropertyGroup" [attr "Condition" $ render c] (genPropertyGroup p) : genContent next
genContent' _ (Free (ItemGroup next)) = genContent next
genContent' _ (Free (Target next)) = genContent next
genContent' _ (Pure _) = []

genPropertyGroup :: PropertyGroupContext -> [Content]
genPropertyGroup (Free (Assignment p v next)) = element (render p) [] [text $ render v] : genPropertyGroup next
genPropertyGroup (Free (PropertyCondition c p next)) = genPropertyGroup' c p ++ genPropertyGroup next
genPropertyGroup (Pure _) = []

genPropertyGroup' :: MSBuildCondition -> PropertyGroupContext -> [Content]
genPropertyGroup' c (Free (Assignment p v next)) = element (render p) [attr "Condition" $ render c] [text $ render v] : genPropertyGroup next
genPropertyGroup' _ (Pure _) = []

class Render a where
    render :: a -> String

instance Render MSBuildProperty where
    render (Property _ s) = s

instance Render MSBuildValue where
    render (StringValue s) = s
    render (ListValue ss) = intercalate ";" ss
    render (PathValue ps) = intercalate "\\" $ fmap render ps
    render (BoolValue True) = "true"
    render (BoolValue False) = "false"
    render (PropertyValue (Property _ n)) = "$(" ++ n ++ ")"

instance Render MSBuildCondition where
    render (Leaf s) = s
    render (PropertyRef (Property _ n)) = "$(" ++ n ++ ")"
    render (ValueRef v) = render v
    render (Or a b) = render a ++ " or " ++ render b
    render (And a b) = render a ++ " and " ++ render b
    render (Equal a b) = "'" ++ render a ++ "' == '" ++ render b ++ "'"
    render (NotEqual a b) = "'" ++ render a ++ "' != '" ++ render b ++ "'"
    render (Exists s) = "exists('" ++ render s ++ "')"

qname :: String -> QName
qname s = QName s Nothing Nothing

attr :: String -> String -> Attr
attr k = Attr $ qname k

element :: String -> [Attr] -> [Content] -> Content
element s as cs = Elem $ Element (qname s) as cs Nothing

text :: String -> Content
text s = Text $ CData CDataText s Nothing
