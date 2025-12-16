{-# OPTIONS_GHC -Wall #-}

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe

repeats :: ByteString -> ByteString -> Integer
repeats = entry where
  entry bs1 bs2
    | len1 == len2 = entryN len1 bs1 bs2
    | otherwise    = -- len2 > len1, as inputs are well-formed
      entryN len1 bs1 (pad9s len1) +
      sum [ entryN n (pad10 n) (pad9s n)
          | n <- [len1 + 1 .. len2 - 1], even n
          ] +
      entryN len2 (pad10 len2) bs2 where
    len1 = B.length bs1; len2 = B.length bs2
    pad9s n = B.replicate n '9'
    pad10 n = B.cons '1' (B.replicate (n - 1) '0')
  entryN n bs1 bs2
    | odd n = 0
    | otherwise = sums where
    sums = (1 - (low - high)) * (low + high) * (10 ^ half + 1) `quot` 2
    half = n `quot` 2
    low  | left < right = left + 1 | otherwise = left where
      left = getLeft bs1; right = getRight bs1
    high | right < left = left - 1 | otherwise = left where
      left = getLeft bs2; right = getRight bs2
    getLeft  = bread . B.take half
    getRight = bread . B.drop half
  bread = fst . fromJust . B.readInteger

main :: IO ()
main = B.interact $ entry 0 . map pairs . B.split ',' where
  pairs = map B.strip . B.split '-'
  entry c ((bs0 : bs1 : _) : bsss) = entry (c + repeats bs0 bs1) bsss
  entry c  _                       = B.pack (show c)
