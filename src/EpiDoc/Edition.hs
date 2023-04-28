{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Edition
    ( edition
    , editionCursor
    , edition'
    , edition''
    , Edition
    , lbs
    , textContent
    ) where

import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc ()
import EpiDoc.TypeClasses hiding (words)
import EpiDoc.Token
import EpiDoc.Lb
import Data.Text as T
import Data.Maybe (mapMaybe)

newtype Edition = Ed {editionCursor :: Cursor}


instance HasTokens Edition where
    tokens :: Edition -> [EpiDoc.Token.Token]
    tokens ed = do 
        let wCursors = xDescCursors (editionCursor ed) "w"
        mapMaybe EpiDoc.Token.create wCursors


instance HasTextContent Edition where
    textContent :: Edition -> String
    textContent = T.unpack . descContent' . editionCursor


edition :: Document -> Edition
edition doc = 
    let editionFilter xs = [d | d <- xs, hasAttrVal d "type" "edition"] in
        EpiDoc.Edition.create . Prelude.head . editionFilter . divNodes $ doc


create :: Cursor -> Edition
create = Ed


edition' :: Document -> Maybe Edition
edition' doc = 
    let editionFilter xs = [d | d <- xs, hasAttrVal d "type" "edition"] in
        create' . Prelude.head . editionFilter . divNodes $ doc


create' :: Cursor -> Maybe Edition
create' c = case localName c of
  Nothing -> Nothing
  Just txt -> case txt of
    "div" -> if hasAttrVal c "type" "edition" then Just (Ed c) else Nothing
    _ -> Nothing


edition'' :: Document -> Maybe Edition
edition'' doc = 
    let editionFilter xs = [d | d <- xs, hasAttrVal d "type" "edition"] in
        create'' =<< (maybeHead . editionFilter . divNodes $ doc)


create'' :: Cursor -> Maybe Edition
create'' c = case localName c of
  Nothing -> Nothing
  Just txt -> case txt of
    "div" -> if hasAttrVal c "type" "edition" then Just (Ed c) else Nothing
    _ -> Nothing


lbs :: Edition -> [Lb]
lbs ed = do 
    let lbCursors = xDescCursors (editionCursor ed) "lb"
    mapMaybe EpiDoc.Lb.create lbCursors