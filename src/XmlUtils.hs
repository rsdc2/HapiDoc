-- For an intro to using xml-conduit / hamlet in Haskell see
-- https://www.yesodweb.com/book/xml (Creative Commons Attribution 4.0 International License)
-- last accessed 2023-04-21
-- which I have found useful in the writing of this module

{-# LANGUAGE OverloadedStrings #-}

module XmlUtils
    ( getElement
    , divNodes
    , maybeElementName
    , maybeLocalName
    , maybeHead
    , localName
    , attrs
    , attrsFromNode
    , attr
    , isAttrInAttrs
    , isAttrValInAttrs
    , isAttrValInCursor
    , isAttrValInNode
    , getAttr
    , hasAttrVal
    , matchLocalName
    , hasLocalName
    , parent
    , xDescCursors
    , descContent
    , writeDoc
    , createTEIDoc
    ) where

import Text.XML
    -- ( Document,
    --   Name(Name, nameLocalName),
    --   Element(elementAttributes, elementName),
    --   Node(NodeElement) )
import Text.XML.Cursor
    ( content, fromDocument, descendant, parent, node, Cursor, Axis)
import Data.Map ( lookup, Map, empty)
import qualified Data.Map as Map

import qualified Data.Text as T
import Text.XML (Prologue)
import Prelude hiding (writeFile)


attr :: T.Text -> Map Name T.Text -> Maybe T.Text
attr a as =
    let n = Name a Nothing Nothing
    in Data.Map.lookup n as


attrs :: Cursor -> Maybe (Map Name T.Text)
attrs c = elementAttributes <$> e
    where e = getElement . node $ c


attrsFromNode :: Node -> Maybe (Map Name T.Text)
attrsFromNode n = elementAttributes <$> e
    where e = getElement n


isAttrInAttrs :: T.Text -> Map Name T.Text -> Bool
isAttrInAttrs a as = attr a as /= Nothing


isAttrValInAttrs :: (T.Text, T.Text) -> Map Name T.Text -> Bool
isAttrValInAttrs (k, v) as = attr k as == Just v


isAttrValInNode :: (T.Text, T.Text) -> Node -> Bool
isAttrValInNode (k, v) (NodeElement (Element _ as _)) = isAttrValInAttrs (k, v) as
isAttrValInNode (_, _) _ = False


isAttrValInCursor :: (T.Text, T.Text) -> Cursor -> Bool
isAttrValInCursor (k, v) c = isAttrValInNode (k, v) $ node c


createTEIDoc :: [Node] -> Document
createTEIDoc ns = Document emptyPrologue (Element "TEI" empty ns) emptyMisc


xDescCursors :: Cursor -> T.Text -> [Cursor]
xDescCursors c nodeLocalName =
    let desc = descendant c in
        [x | x <- desc, hasLocalName nodeLocalName x]


descContent :: Cursor -> T.Text
descContent c = T.concat $ content =<< descendant c 


emptyMisc :: [Miscellaneous]
emptyMisc = []


emptyPrologue :: Prologue
emptyPrologue = Prologue [] Nothing []


getElement :: Node -> Maybe Element
getElement (NodeElement a) = Just a
getElement _ = Nothing


maybeElementName :: Maybe Element -> Maybe Name
maybeElementName (Just a) = Just (elementName a)
maybeElementName Nothing = Nothing


maybeLocalName :: Maybe Name -> Maybe T.Text 
maybeLocalName (Just a) = Just (nameLocalName a)
maybeLocalName Nothing = Nothing


maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead x = Just (head x)


localName :: Cursor -> Maybe T.Text
localName = maybeLocalName . maybeElementName . getElement . node




getAttr :: String -> Map Name T.Text -> Maybe String
getAttr a as = do
    let n = Name (T.pack a) Nothing Nothing
    let r = Data.Map.lookup n as
    T.unpack <$> r


divNodes :: Document -> [Cursor]
divNodes doc = do
    let desc = descendant . fromDocument $ doc in
        [x | x <- desc, hasLocalName "div" x]


hasAttrVal :: Cursor -> T.Text -> T.Text -> Bool
hasAttrVal c a v = do
    let as = attrs c
    let v' = attr a =<< as
    case v' of 
      Nothing -> False
      Just txt -> txt == v

-- hasAttrValNode :: Node -> T.Text -> T.Text -> Bool
-- hasAttrVal n a v = case attrsNode n of
--     | 

    
matchLocalName :: T.Text -> Maybe T.Text -> Bool
matchLocalName s (Just ln) = ln == s 
matchLocalName s Nothing = False 


hasLocalName :: T.Text -> Cursor -> Bool
hasLocalName s c = 
    let ln = localName c
    in matchLocalName s ln


writeDoc :: String -> Document -> IO()
writeDoc = writeFile def


-- getParent :: Cursor -> Maybe Cursor
-- getParent = 