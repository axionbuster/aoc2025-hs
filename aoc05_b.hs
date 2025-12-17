{-# OPTIONS_GHC -Wall #-}
import qualified Data.List as List

merge :: [(Integer, Integer)] -> [(Integer, Integer)]
merge = entry . List.sortOn fst
  where
    entry ((l1, h1) : (l2, h2) : xs)
      | h1 >= l2  =            entry ((l1, max h1 h2) : xs)
      | otherwise = (l1, h1) : entry ((l2,        h2) : xs)
    entry xs = xs

main :: IO ()
main = interact (show . entry [] . lines) where
  entry is ("" : _ ) = sum [h - l + 1 | (l, h) <- merge is]
  entry is (ln : ls) = entry (parseIval ln : is) ls
  entry _        []  = error "no input"
  parseIval s
    | (l, _ : h) <- break (== '-') s = (read l, read h)
    | otherwise                      = error ("bad ival: " ++ s)
