{-# OPTIONS_GHC -Wall #-}

makeDelta :: String -> Integer
makeDelta = entry where
  entry ('L' : numeral) = negate (read numeral)
  entry ('R' : numeral) =         read numeral
  entry  _              = error ""

main :: IO ()
main = interact (entry 0 50 . lines) where
  entry n _  []      = show (n :: Int)
  entry n s (l : ls) = case (s + makeDelta l) `mod` 100 of
    0  -> entry (n + 1) 0  ls
    s' -> entry  n      s' ls
