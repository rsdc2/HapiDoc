{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Edition
    ( edition
    , editionCursor
    , edition'
    , edition''
    , Edition
    , lbs
    ) where

import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc ()
import EpiDoc.TypeClasses hiding (words)
import qualified EpiDoc.Word as W
import EpiDoc.Lb

-- class Wordable a where
--     ws :: a -> [Cursor]


newtype Edition = Ed {editionCursor :: Cursor}


instance W.Wordable Edition where
    ws :: Edition -> [W.W]
    ws ed = do 
        let wCursors = xDescCursors (editionCursor ed) "w"
        fmap W.create wCursors


edition :: Document -> Edition
edition doc = 
    let editionFilter xs = [d | d <- xs, hasAttrVal d "type" "edition"] in
        create . head . editionFilter . divNodes $ doc


create :: Cursor -> Edition
create = Ed


edition' :: Document -> Maybe Edition
edition' doc = 
    let editionFilter xs = [d | d <- xs, hasAttrVal d "type" "edition"] in
        create' . head . editionFilter . divNodes $ doc


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
    fmap EpiDoc.Lb.createLb lbCursors