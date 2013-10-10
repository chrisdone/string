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
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Limit the length of the string and ellipsize it.
ellipsize :: Int -> [Char] -> [Char]
ellipsize n xs
  | length xs > n = take n $ take (max 1 (n-1)) xs ++ "…"
  | otherwise     = xs

-- | A class for converting to strings.
class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id
