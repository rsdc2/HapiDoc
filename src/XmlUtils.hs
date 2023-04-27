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
    , attr
    , getAttr
    , hasAttrVal
    , matchLocalName
    , hasLocalName
    , parent
    , xDescCursors
    , descContent
    , descContent'
    ) where

import Text.XML
import Text.XML.Cursor
import Data.Map
import qualified Data.Text as T


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


attrs :: Cursor -> Maybe (Map Name T.Text)
attrs c = 
    case getElement . node $ c of 
        Just el -> Just (elementAttributes el)
        Nothing -> Nothing


attr :: T.Text -> Maybe (Map Name T.Text) -> Maybe T.Text
attr a as =
    case as of
      Nothing -> Nothing
      Just mp -> do
        let n = Name a Nothing Nothing
        Data.Map.lookup n mp


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
    let v' = attr a as
    case v' of 
      Nothing -> False
      Just txt -> txt == v

    
matchLocalName :: T.Text -> Maybe T.Text -> Bool
matchLocalName s (Just ln) = ln == s 
matchLocalName s Nothing = False 


hasLocalName :: T.Text -> Cursor -> Bool
hasLocalName s c = do
    let ln = localName c
    matchLocalName s ln


xDescCursors :: Cursor -> T.Text -> [Cursor]
xDescCursors c nodeLocalName =
    let desc = descendant c in
        [x | x <- desc, hasLocalName nodeLocalName x]


descContent :: Cursor -> T.Text
descContent c = do
    let descs = descendant c
    let c = concat . Prelude.map content $ descs
    -- let c = [content d | d <- descs]
    let t = T.concat c   
    t

descContent' :: Cursor -> T.Text
-- descContent' c = T.concat $ concat . Prelude.map content . descendant $ c 
descContent' c = T.concat $ content =<< descendant c

-- getParent :: Cursor -> Maybe Cursor
-- getParent = 