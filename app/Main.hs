module Main (main) where

{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile)
import Text.XML
import Text.XML.Cursor
import Data.Function
import Data.Data

import qualified Data.Text as T

import qualified EpiDoc.Token as Token
import qualified EpiDoc.Lb as Lb
import XmlUtils(writeDoc, createTEIDoc)
import EpiDoc.TypeClasses (HasTextContent(textContent))
-- import Control.Applicative (Alternative(some)) 
import EpiDoc.Edition (Edition(..), ElemType(..), text, w, toNodes, tokenize, fromNodes, fromDoc)
import EpiDoc.XMLEdition (XMLEdition(..), editionTemplate, replaceEditionInDoc)


-- isicFunc :: IO ()
-- isicFunc = do
--     doc <- readFile def "ISic000001.xml"
--     let descs = descendant . E.editionCursor . E.edition $ doc
--     print $ T.concat $ content =<< descs
--         -- divNode doc & head & descendant >>= content
--         -- content . head . descendant . head $ divNode doc
--     -- let cursor = fromDocument doc
--     -- print . maybeLocalName . maybeElementName . getNodeElement . node $ cursor


-- isicFunc' :: IO ()
-- isicFunc' = do
--     doc <- readFile def "ISic000001.xml"
--     let contentMap xs = [content x | x <- xs]
--     -- let cont = fmap concat . fmap contentMap . fmap descendant . fmap E.editionCursor . E.edition' $ doc
--     -- let cont = fmap (concat . contentMap . descendant . E.editionCursor) . E.edition' $ doc
--     let cont = concat . contentMap . descendant . E.editionCursor <$> E.edition'' doc
--     print $ T.concat <$> cont


-- isicFunc'' :: IO ()
-- isicFunc'' = do
--     doc <- readFile def "ISic000001.xml"
--     let contentMap xs = [textContent x | x <- xs]
--     -- let firstW = head . E.ws . E.edition $ doc
--     let ws = Token.tokens . E.edition $ doc
--     let ds = contentMap ws

--     -- let cont = ds >>= content
--     print $ ds


-- applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe Nothing f = Nothing
-- applyMaybe (Just x) f = f x


-- parents :: IO()
-- parents = do
--     doc <- readFile def "ISic000001.xml"
--     let ws = Token.tokens . E.edition $ doc
--     let parents' = head . map localName . parent . head $ [Token.cursor w | w <- ws]
--     print parents'


-- lbs :: IO()
-- lbs = do
--     doc <- readFile def "ISic000001.xml"
--     let lbs = E.lbs . E.edition $ doc
--     let firstLb = head $ lbs
--     let tokens = Lb.tokens firstLb
--     let c = map show tokens
--     print c
--     -- print firstLb


-- editionText :: IO()
-- editionText = do
--     doc <- readFile def "ISic000001.xml"
--     putStrLn $ textContent . E.edition $ doc


createEdition :: Edition
createEdition = w "Deus" (text "Dis") <> w "Manes" (text "Manibus") <> text "vixit"


tokenizeEdition :: IO ()
tokenizeEdition = writeDoc "test.xml" $ createTEIDoc [editionTemplate nodes] where
            nodes = toNodes (tokenize createEdition)


editionFromXML :: IO ()
editionFromXML = do
    doc <- readFile def "test.xml"
    let ed = EpiDoc.Edition.fromDoc doc
    let tokenized = tokenize ed
    let newDoc = createTEIDoc [editionTemplate nodes] where
            nodes = toNodes tokenized
    writeDoc "tokenized.xml" newDoc
    print tokenized


replaceEdition :: IO ()
replaceEdition = do
    doc <- readFile def "ISic000001.xml"
    let newDoc = replaceEditionInDoc doc
    writeDoc "no_edition.xml" newDoc


main :: IO ()
main = replaceEdition