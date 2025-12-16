{-# OPTIONS_GHC -Wall #-}
import qualified Data.Array as A

movables :: A.Array (Int, Int) Cell -> Int
movables cells  = sum [1 | rc <- A.indices cells, oc rc, nearby rc < (4 :: Int)] where
  nearby (r, c) = sum [1 | xy <- ns, A.inRange (A.bounds cells) xy, oc xy] where
    ns = [(i + r, j + c) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]
  oc rc = cells A.! rc == '@'

type Cell = Char

main :: IO ()
main = interact (show . movables . parse . lines) where
  parse ls@(hl : _) = A.listArray ((1, 1), (length ls, length hl)) (concat ls)
  parse _           = error "no input"
