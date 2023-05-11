{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Edition
    ( 
        Edition(..),
        ElemType(..),
        text,
        -- Boundary,
        -- newBoundary,
        w,
        show,
        toNodes


    ) where

-- import Text.XML
import Text.XML
import Text.XML.Cursor ()
import qualified Data.Text as T
import Data.Maybe(fromMaybe)
import Utils(quote, paren, toMaybe, curly)
import XmlUtils (localName, descContent)
import EpiDoc.TypeClasses 
    (     
        HasTextContent(..),
        HasCursor(..),
        XMLable(..),
        HasXMLName(..)
    )
import Data.Map(empty)


data ElemType = 
      W (Maybe String) -- W Lemma
    | Name' 
    | Num
    | Lb (Maybe String) -- Lb LineNo
    | G (Maybe String) -- G Mark


instance Show ElemType where
    show :: ElemType -> String
    show (W l) = "w{" <> maybe "" quote l
    show Name' = "name{"
    show Num = "num{"
    show (Lb n) = "lb{" <> maybe "" quote n 
    show (G m) = "g{" <> maybe "" quote m


instance HasXMLName ElemType where
    tagName :: ElemType -> Name
    tagName (W _) = Name "w" Nothing Nothing
    tagName Name' = Name "name" Nothing Nothing
    tagName Num  = Name "num" Nothing Nothing
    tagName (Lb _) = Name "lb" Nothing Nothing
    tagName (G _) = Name "g" Nothing Nothing

-- data TokenType = 
--       W {lemma :: String, pos :: String}
--     | Name 
--     | Num
--     deriving Show

data CompoundTokenType = PersName | OrgName
    deriving Show

data SubTokenType = Expan
    deriving Show

-- data BoundaryType = 
--       Lb {number :: String} 
--     | G

data ContainerType = Ab | Div


data Edition = 
      EditionSeq [Edition] 
    | EditionElem ElemType Edition
    | EditionText T.Text
    | Empty


-- data Edition l a where
--     Edition :: (EditionElem a, Functor l) => l a -> Edition l a

-- data Edition a = Edition [EditionElem]


instance Semigroup Edition where
    (<>) :: Edition -> Edition -> Edition
    -- Edition e1s <> Edition e2s = Edition (e1s <> e2s)
    EditionSeq e1s <> EditionSeq e2s = EditionSeq (e1s <> e2s)
    EditionSeq es <> e = EditionSeq (es <> [e]) 
    e <> EditionSeq es = EditionSeq ([e] <> es) 
    Empty <> e = e
    e1 <> e2 = EditionSeq [e1, e2]

    -- Token n t e1s <> EditionSeq e2s = EditionSeq ([Token n t e1s] <> e2s)
    -- Token n t es <> e = EditionSeq [Token n t es, e]
    -- Subtoken n t es <> e = EditionSeq [Subtoken n t es, e]
    -- Boundary n no <> e = EditionSeq [Boundary n no, e]
    -- Container n es <> e = EditionSeq [Container n es, e]
    -- EditionText t <> e = EditionSeq [EditionText t, e]


instance Monoid Edition where
    mempty :: Edition
    mempty = Empty


instance HasTextContent Edition where
    descTextContent :: Edition -> T.Text
    descTextContent (EditionSeq es) = foldr (<>) "" [textContent e | e <- es]
    descTextContent (EditionText s) = s 
    descTextContent (EditionElem _ e) = textContent e
    descTextContent Empty = T.pack ""

    textContent :: Edition -> T.Text
    textContent (EditionText s) = s
    textContent (EditionSeq es) = foldr (<>) "" [textContent e | e <- es]
    textContent (EditionElem _ e) = textContent e
    textContent Empty = ""

-- instance Show (Edition a) where
--     show :: Edition a -> String
--     show (Edition es) = "[" <> unwords (show <$> es) <> "]"

instance Show Edition where
    show :: Edition -> String
    -- show (EditionSeq es) = "[" <> unwords (show <$> es) <> "]"
    -- show (EditionElem t e) = "Token('" <> T.unpack t <> "')"
    -- show (EditionText x) = x
    -- show _ = "..."
    show (EditionSeq es) = "[" <> unwords (show <$> es) <> "]"
    show (EditionElem t e) = show t <> " " <> show e <> "}"
    show (EditionText x) = "text" <> (paren . quote $ T.unpack x) 
    -- show (EditionText x) = quote $ T.unpack x 
    show _ = ""


instance XMLable Edition where
    toNodes :: Edition -> [Node]
    toNodes (EditionSeq es) = es >>= toNodes
    toNodes (EditionElem t e) = [NodeElement $ Element (tagName t) empty (toNodes e)]
    toNodes (EditionText x) = [NodeContent x]
    toNodes _ = []


-- instance Show EditionElem where
--     show :: EditionElem -> String
--     show (Token n t es) = "Token('" <> T.unpack t <> "')"
--     show (Boundary n no) = " | "
--     show _ = "..."


-- instance HasCursor Edition where
--     -- cursor :: Edition -> Cursor
--     -- cursor 

--     create :: Cursor -> Maybe Edition
--     create c = case localName c of
--         Just "w" -> Just (Word (T.unpack $ descContent c) (create <$> descendant c))
--         Just "num" -> Just (Num c)
--         Just "name" -> Just (EpiDoc.EpiDoc.Name c)
--         _ -> Nothing



w :: String -> Edition -> Edition
w l = EditionElem . W $ toMaybe "" l
    
text :: String -> Edition
text s = EditionText (T.pack s)

-- newBoundary :: String -> String -> Edition
-- newBoundary n no = EditionElem (T.pack n) (T.pack no)