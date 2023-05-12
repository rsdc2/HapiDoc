{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module EpiDoc.Edition
    ( 
        Edition(..),
        ElemType(..),
        text,
        w,
        show,
        toNodes, fromNodes, fromDoc,
        tokenize
    ) where

import Text.XML(Document(..), Element(..), Node(..), Name(..))
import qualified Data.Text as T
import Data.Maybe(fromMaybe)
import Utils(quote, paren, toMaybe, curly)
import XmlUtils (localName, descContent)
import EpiDoc.TypeClasses 
    ( HasTextContent(..)
    , HasCursor(..)
    , XMLable(..)
    , HasXMLName(..)
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

    fromName :: Name -> ElemType
    fromName (Name "w" _ _) = W Nothing
    fromName (Name "name" _ _) = Name'
    fromName (Name "num" _ _) = Num
    fromName (Name "lb" _ _) = Lb Nothing
    fromName (Name "g" _ _) = G Nothing
    fromName (Name {}) = W Nothing


data CompoundTokenType = PersName | OrgName
    deriving Show


data SubTokenType = Expan
    deriving Show


data ContainerType = Ab | Div


data Edition = 
      EditionSeq [Edition] 
    | EditionElem ElemType Edition
    | EditionText T.Text
    | Empty


instance Semigroup Edition where
    (<>) :: Edition -> Edition -> Edition
    EditionSeq e1s <> EditionSeq e2s = EditionSeq (e1s <> e2s)
    EditionSeq es <> e = EditionSeq (es <> [e]) 
    e <> EditionSeq es = EditionSeq ([e] <> es) 
    Empty <> e = e
    e1 <> e2 = EditionSeq [e1, e2]


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


instance Show Edition where
    show :: Edition -> String
    show (EditionSeq es) = "[" <> unwords (show <$> es) <> "]"
    show (EditionElem t e) = show t <> " " <> show e <> "}"
    show (EditionText x) = "text" <> (paren . quote $ T.unpack x) 
    show _ = ""


instance XMLable Edition where
    toNodes :: Edition -> [Node]
    toNodes (EditionSeq es) = es >>= toNodes
    toNodes (EditionElem t e) = [NodeElement $ Element (tagName t) empty (toNodes e)]
    toNodes (EditionText x) = [NodeContent x]
    toNodes _ = []


fromNodes :: [Node] -> Edition
fromNodes [x] = fromNode x
fromNodes ns = EditionSeq (fromNode <$> ns)


fromNode :: Node -> Edition
fromNode (NodeElement (Element "div" _ ns)) = fromNodes ns
fromNode (NodeElement (Element name _ ns)) = EditionElem (fromName name) (fromNodes ns)
fromNode (NodeContent s) = EditionText s
fromNode _ = Empty


fromDoc :: Document -> Edition
fromDoc (Document _ (Element "TEI" _ ns) _) = fromNodes ns
fromDoc _ = Empty


w :: String -> Edition -> Edition
w l = EditionElem . W $ toMaybe "" l


tokenize :: Edition -> Edition
tokenize (EditionText s) = EditionElem (W Nothing) (EditionText s)
tokenize (EditionSeq es) = EditionSeq (tokenize <$> es)
tokenize (EditionElem t (EditionText s)) = EditionElem t (EditionText s)
tokenize (EditionElem t e) = EditionElem t (tokenize e)
tokenize Empty = Empty

    
text :: String -> Edition
text s = EditionText (T.pack s)
