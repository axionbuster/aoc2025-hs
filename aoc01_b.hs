{-# OPTIONS_GHC -Wall #-}
import           Debug.Trace
import           Text.Printf

makeDelta :: String -> Integer
makeDelta = entry where
  entry ('L' : numeral) = negate (read numeral)
  entry ('R' : numeral) =         read numeral
  entry  _              = error ""

main :: IO ()
main = interact (entry 0 50 . lines) where
  tr n s s' l m = trace (printf "n = %4v -> s = %4v + (l = %4v) = %4v -> n + %4v" n s l s' m)
  entry n _ []       = show (n :: Integer)
  entry n s (l : ls) = tr n s t l q $ entry (n + q) r ls where
    d = makeDelta l
    t = s + d
    r = t `mod` 100
    q | s > 0, t <= 0 = abs (t `quot` 100) + 1
      | otherwise     = abs (t `quot` 100)
