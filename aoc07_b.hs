{-# OPTIONS_GHC -Wall #-}
import           Data.Array
import qualified Data.ByteString.Char8 as B
import           Data.Graph
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IntMap
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as IntSet
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import           Debug.Trace

main :: IO ()
main = B.interact (B.pack . show @Integer . entry . deinterlace . B.lines) where
  entry (_ : rest) = solve $ build $ abbrev $ B.transpose rest
  entry      _     = error "invalid input"
  solve gi@(GraphInfo gr _ st) | rts <- reverse (topSort gr) = let
    -- Count paths from st to any end node via dynamic programming on DAG
    dp = array (bounds gr)
         [ (k, max 1 $ sum (map (dp !) (gr ! k)))
         |  k <- rts
         ] in
    trace (show gi) $ dp ! st
  abbrev = IntMap.fromList . zip [0..] . map (IntSet.fromList . B.elemIndices '^')
  deinterlace (x : _ : xs) = x : deinterlace xs
  deinterlace          xs  = xs

data I
  = Wedge Int Int -- ^ Position ... row and column, resp.
  | End       Int -- ^ Final row reached at column
  deriving (Eq, Ord, Show)

data GraphInfo
  = GraphInfo
    Graph     -- ^ Graph structure
   (IntMap I) -- ^ Names of vertices
    Int       -- ^ Starting vertex index

-- graphviz format, directed
instance Show GraphInfo where
  show (GraphInfo g names _) = init $ unlines $
    "digraph G {" :
    [ "  " ++ na a ++ ";" | a <- vertices g, not (isolated a) ] ++
    [ "  " ++ na a ++ " -> " ++ na b ++ ";"
    | (a, bs) <- assocs g, b <- bs
    ] ++
    ["}"] where
      na = na' . (names IntMap.!)
      na' (Wedge r c) = "\"(" ++ show r ++ "," ++ show c ++ ")\""
      na' (End     c) = show c
      isolated a = indegree g ! a == 0 && outdegree g ! a == 0

build :: IntMap IntSet -> GraphInfo
build wedges = GraphInfo graph names start where
  list  = [Wedge r c | (c, rs) <- IntMap.toList wedges, r <- IntSet.toList rs]
  graph = fmap Set.toList $ accumArray (flip Set.insert) mempty (0, count - 1) $ do
    Wedge r c <- list
    j         <- [c - 1, c + 1] >>= march r >>= mtl . (arind Map.!?)
    pure (arind Map.! Wedge r c, j)
  items = list ++ map End (IntMap.keys wedges)
  count = IntMap.size names
  names = IntMap.fromList $ zip [0..] items
  arind =    Map.fromList $ zip items [0..]
  march r c
    | Just r' <- IntMap.lookup c wedges >>= IntSet.lookupGT r = [Wedge r' c]
    | otherwise = [End c]
  start = snd $ Map.findMin arind
  mtl (Just a) = [a]
  mtl       _  = [ ]
