{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.Creatable
    ( Creatable
    , create'''
    ) where

import EpiDoc.Edition
import EpiDoc.Word
import Text.XML.Cursor

data EpiDocNode = Edition | Word

class Creatable c where
    create''' :: c -> Maybe EpiDocNode
