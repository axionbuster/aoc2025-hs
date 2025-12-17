{-# OPTIONS_GHC -Wall #-}

main :: IO ()
main = interact (show @Int . entry [] . lines) where
  entry is ("" : re) = questions is (read @Integer <$> re)
  entry is (ln : ls) = entry (parseIval ln : is) ls
  entry _        []  = error "no input"
  questions is qs  = sum [1 | q <- qs, any (overlap q) is]
  overlap q (l, h) = l <= q && q <= h
  parseIval s
    | (l, _ : h) <- break (== '-') s = (read l, read h)
    | otherwise                      = error ("bad ival: " ++ s)
