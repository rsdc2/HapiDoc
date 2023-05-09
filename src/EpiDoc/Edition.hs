{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Edition
    ( 
        Edition,
        Token,
        -- Boundary,
        newBoundary,
        newToken,
        show

    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor ()
import EpiDoc.Token(Token)
import qualified Data.Text as T

import XmlUtils (localName, descContent)
import EpiDoc.TypeClasses (HasTextContent(..), HasCursor(..))
import Data.List (intercalate)

data Edition = 
      EditionSeq [Edition]
    | Token {name :: T.Text, text :: T.Text, children :: [Edition]}
    | Subtoken {name :: T.Text, text :: T.Text, children :: [Edition]}
    | Boundary {name :: T.Text, number :: T.Text}
    | Container {name :: T.Text, children :: [Edition]}
    | EditionText T.Text
    | Empty


instance Semigroup Edition where
    (<>) :: Edition -> Edition -> Edition
    EditionSeq es <> e = EditionSeq (es <> [e]) 
    Token n t es <> e = EditionSeq [Token n t es, e]
    Subtoken n t es <> e = EditionSeq [Subtoken n t es, e]
    Boundary n no <> e = EditionSeq [Boundary n no, e]
    Container n es <> e = EditionSeq [Container n es, e]
    EditionText t <> e = EditionSeq [EditionText t, e]
    Empty <> e = e


instance Monoid Edition where
    mempty :: Edition
    mempty = Empty


instance HasTextContent Edition where
    textContent :: Edition -> T.Text
    textContent (EditionSeq es) = foldr (<>) "" [textContent e | e <- es]
    textContent (Token _ t es) = t <> textContent (EditionSeq es)
    textContent (Subtoken _ t es) = t <> textContent (EditionSeq es)
    textContent (Container _ es) = textContent (EditionSeq es)
    textContent (Boundary _ _) = ""
    textContent (EditionText t) = t
    textContent Empty = ""


instance Show Edition where
    show :: Edition -> String
    show (Token n t es) = "Token('" <> T.unpack t <> "')"
    show (EditionSeq es) = unwords $ show <$> es
    show (Boundary n no) = " | "
    show _ = "..."

-- instance HasCursor Edition where
--     -- cursor :: Edition -> Cursor
--     -- cursor 

--     create :: Cursor -> Maybe Edition
--     create c = case localName c of
--         Just "w" -> Just (Word (T.unpack $ descContent c) (create <$> descendant c))
--         Just "num" -> Just (Num c)
--         Just "name" -> Just (EpiDoc.EpiDoc.Name c)
--         _ -> Nothing



newToken :: String -> String -> [Edition] -> Edition
newToken n t = Token (T.pack n) (T.pack t)
    

newBoundary :: String -> String -> Edition
newBoundary n no = Boundary (T.pack n) (T.pack no)