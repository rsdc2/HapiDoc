{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Lb
    (Lb
    , create
    , tokens
    , show
    , cursor
    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor
import XmlUtils
import EpiDoc.EpiDoc
import EpiDoc.TypeClasses
import Data.Maybe (isJust, mapMaybe)
import EpiDoc.Token (HasTokens, Token, tokens, create)
import qualified Data.Text as T

data Lb = Lb Cursor


instance HasTokens Lb where
    tokens :: Lb -> [Token]
    tokens lb = do        
        let sibs = followingSibling . cursor $ lb
        mapMaybe EpiDoc.Token.create sibs
        
        
instance HasCursor Lb where
    cursor :: Lb -> Cursor
    cursor (Lb a) = a

    create :: Cursor -> Maybe Lb
    create c = Just (Lb c)


instance Show Lb where
    show :: Lb -> String
    show lb = do
        let n = numberStr lb
        let s = show n
        "lb(n=" ++ s ++ ")"


instance HasNumber Lb where
    numberStr :: Lb -> Maybe String
    numberStr lb = do
        let c = cursor lb
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

