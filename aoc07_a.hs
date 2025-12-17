{-# OPTIONS_GHC -Wall #-}
import           Data.List  (elemIndex, elemIndices, intersect, nub, (\\))
import           Data.Maybe

main :: IO ()
main = interact (show . entry . lines) where
  entry (l : _ : ls) = go [fromJust $ elemIndex 'S' l] ls
  entry          _   = error "invalid input"
  go beams (l : _ : ls) = increase + go beams' ls where
    beams'   = nub $ (beams ++ splits) \\ wedges
    increase = length $ intersect wedges beams
    splits   = wedges >>= \i -> [i - 1, i + 1]
    wedges   = elemIndices '^' l
  go _ _ = 0
