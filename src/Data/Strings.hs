{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Extra functions and classes for dealing with Strings.

module Data.Strings
  (module Data.String
  ,module Data.Char
  ,lower
  ,upper
  ,trim
  ,ToString(..))
  where

import Data.Char
import Data.String

-- | Lower case a string.
lower :: String -> String
lower = map toLower

-- | Upper case a string.
upper :: String -> String
upper = map toUpper

-- | Trim a string.
trim :: [Char] -> [Char]
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | A class for converting to strings.
class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id
