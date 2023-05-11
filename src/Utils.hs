{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( 
      quote
    , paren
    , toMaybe
    , curly
    ) where

quote :: String -> String
quote s = "'" <> s <> "'"

paren :: String -> String
paren s = "(" <> s <> ")"

curly :: String -> String
curly s = "{" <> s <> "}"

-- Reverse of built-in maybe function
-- Converts a default value into a Nothing
-- Otherwise returns a Just 
toMaybe :: (Eq a) => a -> a -> Maybe a
toMaybe d v = if d == v then Nothing else Just v
