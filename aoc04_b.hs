{-# OPTIONS_GHC -Wall #-}
import qualified Data.Array as A

-- remove movables from the grid and count them
step :: Cells -> (Int, Cells)
step cells = (length changes, cells A.// changes) where
  changes = [(rc, '.')   | rc <- A.indices cells, oc rc, nearby rc < (4 :: Int)]
  nearby (r, c) = sum [1 | xy <- ns, A.inRange (A.bounds cells) xy, oc xy] where
    ns = [(i + r, j + c) | i <- [-1 .. 1], j <- [-1 .. 1], (i, j) /= (0, 0)]
  oc rc = cells A.! rc == '@'

-- repeatedly remove movables until none remain, counting how many were removed
fixCells :: Cells -> Int
fixCells = go 0 where
  go n cs = case step cs of
    (0, _  ) -> n
    (m, cs') -> go (n + m) cs'

type Cell  = Char
type Cells = A.Array (Int, Int) Cell

main :: IO ()
main = interact (show . fixCells . parse . lines) where
  parse ls@(hl : _) = A.listArray ((1, 1), (length ls, length hl)) (concat ls)
  parse _           = error "no input"
