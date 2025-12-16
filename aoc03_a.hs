{-# OPTIONS_GHC -Wall #-}

-- NOTE: digits must be given in reverse order (least significant first)
jolt2 :: (Ord a) => [a] -> [a]
jolt2 = entry where
  entry [x,  y]          = [y, x]
  entry (x : y : z : ws) = go x y (z : ws)
  entry  _               = error "input too short"
  -- go (one's place) (max ten's place) (ten's place)
  go o s (t : ts)
    | s <= t    = go (max s o) t ts
    | otherwise = go        o  s ts
  go o s [] = [s, o]

main :: IO ()
main = interact (show @Int . sum . map (read . jolt2 . reverse) . lines)
