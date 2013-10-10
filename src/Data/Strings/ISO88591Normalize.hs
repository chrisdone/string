module Data.Strings.ISO88591Normalize where

import           Data.Char
import           Data.Map   (Map)
import qualified Data.Map   as M

-- | Normalize the string.
normalize :: String -> String
normalize = concat . map replace where
  replace c = maybe [c] restoreCase $ M.lookup (toLower c) mapping
    where restoreCase | isUpper c = map toUpper
                      | otherwise = id

-- | Mapping that will be normalized.
mapping :: Map Char String
mapping = foldr (\(to,froms) m -> foldr (`M.insert` to) m froms) M.empty
  [("D",['Ð'])
  ,("o",['ð','ò','ó','ô','õ','ö','ø','ō'])
  ,("ss",['ß'])
  ,("a",['à','á','â','ã','ä','å'])
  ,("ae",['æ'])
  ,("c",['ç'])
  ,("e",['è','é','ê','ë'])
  ,("i",['ì','í','î','ï'])
  ,("n",['ñ'])
  ,("u",['ù','ú','û','ü'])
  ,("y",['ý','ÿ'])]
