{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Word
    ( W
    , cursor
    , ws
    , Wordable
    , wContent
    , create
    , show
    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc
import EpiDoc.TypeClasses
import qualified Data.Text as T


class Wordable a where
    ws :: a -> [W]
    


data W = W {wordCursor :: Cursor}


instance WContentable W where
    wContent :: W -> T.Text
    wContent = descContent . wordCursor

instance Show W where
    show :: W -> String
    show w = "w"


create :: Cursor -> W
create = W


cursor :: W -> Cursor
cursor = wordCursor 
