module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import Data.Function
import Data.Data

import qualified Data.Text as T

import qualified EpiDoc.Edition as E
import qualified EpiDoc.Word as W
import qualified EpiDoc.Lb as Lb
import XmlUtils
import EpiDoc.TypeClasses (WContentable(wContent))
import Control.Applicative (Alternative(some)) 


isicFunc :: IO ()
isicFunc = do
    doc <- readFile def "ISic000001.xml"
    let descs = descendant . E.editionCursor . E.edition $ doc
    print $ T.concat $ descs >>= content
        -- divNode doc & head & descendant >>= content
        -- content . head . descendant . head $ divNode doc
    -- let cursor = fromDocument doc
    -- print . maybeLocalName . maybeElementName . getNodeElement . node $ cursor


isicFunc' :: IO ()
isicFunc' = do
    doc <- readFile def "ISic000001.xml"
    let contentMap xs = [content x | x <- xs]
    -- let cont = fmap concat . fmap contentMap . fmap descendant . fmap E.editionCursor . E.edition' $ doc
    -- let cont = fmap (concat . contentMap . descendant . E.editionCursor) . E.edition' $ doc
    let cont = concat . contentMap . descendant . E.editionCursor <$> E.edition'' doc
    print $ T.concat <$> cont


isicFunc'' :: IO ()
isicFunc'' = do
    doc <- readFile def "ISic000001.xml"
    let contentMap xs = [wContent x | x <- xs]
    -- let firstW = head . E.ws . E.edition $ doc
    let ws = W.ws . E.edition $ doc
    let ds = contentMap ws

    -- let cont = ds >>= content
    print $ ds


applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x


parents :: IO()
parents = do
    doc <- readFile def "ISic000001.xml"
    let ws = W.ws . E.edition $ doc
    let parents' = head . map localName . parent . head $ [W.cursor w | w <- ws]
    print parents'

lbs :: IO()
lbs = do
    doc <- readFile def "ISic000001.xml"
    let contentMap xs = [wContent x | x <- xs]
    let lbs = E.lbs . E.edition $ doc
    let firstLb = head . tail $ lbs
    let words = Lb.ws firstLb
    let content = contentMap words
    print content


main :: IO ()
-- main = isicFunc''
main = lbs