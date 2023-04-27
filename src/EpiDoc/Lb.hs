{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Lb
    (Lb
    , createLb
    , tokens
    , show
    , lbCursor
    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc
import EpiDoc.TypeClasses
import EpiDoc.Token
import qualified Data.Text as T

data Lb = Lb {lbCursor :: Cursor}


instance HasTokens Lb where
    tokens :: Lb -> [Token]
    tokens lb = do 
        let tokenFilter e = localName e == Just "w"
        let sibs = followingSibling . lbCursor $ lb
        let filtered = filter tokenFilter sibs
        [EpiDoc.Token.create e | e <- filtered]
        
        
instance Show Lb where
    show :: Lb -> String
    show lb = do
        let n = numberStr lb
        let s = show n
        "lb(n=" ++ s ++ ")"


instance HasNumber Lb where
    numberStr :: Lb -> Maybe String
    numberStr lb = do
        let c = lbCursor lb
        let as = attrs c
        let fNumber = getAttr "n"
        fNumber =<< as

    numberInt :: Lb -> Maybe Int
    numberInt lb = read <$> numberStr lb


instance Ord Lb where 
    (<) :: Lb -> Lb -> Bool
    a < b = numberInt a < numberInt b  
    (>) :: Lb -> Lb -> Bool
    a > b = numberInt a > numberInt b
    (<=) :: Lb -> Lb -> Bool
    a <= b = numberInt a <= numberInt b


instance Eq Lb where
    (==) :: Lb -> Lb -> Bool
    a == b = numberInt a == numberInt b


cursor :: Lb -> Cursor
cursor = lbCursor 

createLb :: Cursor -> Lb
createLb = Lb