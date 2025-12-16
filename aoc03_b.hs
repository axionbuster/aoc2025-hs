{-# OPTIONS_GHC -Wall #-}
import qualified Data.Array as A

-- NOTE: digits must be given in reverse order (least significant first)
jolt :: (Ord a) => Int -> [a] -> [a]
jolt n = entry where
  entry dgts = go (n - 1) l [] (A.listArray (0, l - 1) dgts A.!) where l = length dgts
  go (-1) _ b _ = reverse b
  go   s  e b d = go (s - 1) i (d i : b) d where
    i = foldl' (\k m -> if d k > d m then k else m) s [s + 1 .. e - 1]

main :: IO ()
main = interact (show @Integer . sum . map (read . jolt 12 . reverse) . lines)
