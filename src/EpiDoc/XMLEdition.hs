{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.XMLEdition
    ( XMLEdition(..)
    , editionTemplate
    , replaceEditionInDoc
    ) where

import Text.XML
import Text.XML.Cursor
import XmlUtils(isAttrValInNode, isAttrInAttrs) 
import XmlUtils hiding (attrs)

import EpiDoc.TypeClasses hiding (words)
import EpiDoc.Token
import EpiDoc.Lb
import Data.Text as T hiding(head)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Map as Map

newtype XMLEdition = XMLEd {editionCursor :: Cursor}


instance HasTokens XMLEdition where
    tokens :: XMLEdition -> [EpiDoc.Token.Token]
    tokens ed = let wCursors = xDescCursors (editionCursor ed) "w"
        in mapMaybe EpiDoc.Token.create wCursors


instance HasTextContent XMLEdition where
    textContent :: XMLEdition -> T.Text
    textContent = descContent . editionCursor
    

edition :: Document -> Maybe XMLEdition
edition doc = 
    let editionFilter xs = [d | d <- xs, hasAttrVal d "type" "edition"] 
    in EpiDoc.XMLEdition.create . Prelude.head . editionFilter . divNodes $ doc


replaceEditionInDoc :: Document -> Document
replaceEditionInDoc (Document p (Element name attrs ns) ep) = Document p (Element name attrs (replaceEditionNode (editionTemplate []) <$> ns)) ep
replaceEditionInDoc d = d

    -- let c = editionCursor <$> edition d
    -- let c' = fromNode $ editionTemplate []
    -- let p = parent <$> c
    -- let pNode = node . head $ fromMaybe [] p
    -- let editionNode = node <$> c
    -- let editionNode' = node c'

    

replaceEditionNode :: Node -> Node -> Node
replaceEditionNode n' n = if isEdition n then n' else n


replaceTextNode :: Node -> Node -> Node
replaceTextNode n' (NodeElement (Element "text" _ _)) = editionTemplate []
replaceTextNode _ n = n


replaceChildren :: Node -> [Node] -> Node
replaceChildren (NodeElement (Element name attrs _)) ns' = NodeElement (Element name attrs ns')
replaceChildren n _ = n 


-- editionNode :: Document -> Maybe Node
-- editionNode d = node . editionCursor <$> edition d


editionNode :: [Node] -> Node
editionNode ns = NodeElement $ Element "div" (Map.fromList [("type", "edition")]) ns


isEdition :: Node -> Bool
isEdition (NodeElement (Element "div" attrs _)) = isAttrValInAttrs ("type", "edition") attrs
isEdition _ = False


editionTemplate :: [Node] -> Node
editionTemplate ns = NodeElement $ Element "div" (Map.fromList [("type", "edition")]) ns


create :: Cursor -> Maybe XMLEdition
create c = case localName c of
  Nothing -> Nothing
  Just txt -> case txt of
    "div" -> if hasAttrVal c "type" "edition" then Just (XMLEd c) else Nothing
    _ -> Nothing


-- edition'' :: Document -> Maybe Edition
-- edition'' doc = 
--     let editionFilter xs = [d | d <- xs, hasAttrVal d "type" "edition"] in
--         create'' =<< (maybeHead . editionFilter . divNodes $ doc)


-- create'' :: Cursor -> Maybe Edition
-- create'' c = case localName c of
--   Nothing -> Nothing
--   Just txt -> case txt of
--     "div" -> if hasAttrVal c "type" "edition" then Just (Ed c) else Nothing
--     _ -> Nothing


lbs :: XMLEdition -> [Lb]
lbs ed = do 
    let lbCursors = xDescCursors (editionCursor ed) "lb"
    mapMaybe EpiDoc.Lb.create lbCursors