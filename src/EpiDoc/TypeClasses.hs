{-# LANGUAGE OverloadedStrings #-}

module EpiDoc.TypeClasses
    ( HasTextContent
    , textContent
    , HasNumber
    , numberStr
    , numberInt
    ) where


class HasTextContent a where
    textContent :: a -> String


class HasNumber a where
    numberStr :: a -> Maybe String
    numberInt :: a -> Maybe Int
