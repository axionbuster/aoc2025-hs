{-# OPTIONS_GHC -Wall #-}
import           Control.Monad
import           Data.Array                   (accumArray, bounds)
import qualified Data.Array                   as A
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Graph
import qualified Data.IntMap.Strict           as IntMap
import           Data.List
import qualified Data.Map.Strict              as Map
import           Data.Maybe
import           Data.Vector.Unboxed          ((!))
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as M
import           GHC.Exts                     (the)
import           Text.ParserCombinators.ReadP hiding (count)
data GraphInfo key = GI
  { giGraph  :: Graph
  , giVertex :: Vertex -> (key, [key])
  , giKey    :: key    -> Maybe Vertex
  }
type Adj0 t = (t, [t])
type Adj  t = [Adj0 t]
-- Make a DAG out of the nodes. Assume it's a DAG.
-- the graphFromEdges function has a problem, where it ignores vertices with no outgoing edges,
-- such as 'out' in the problem input.
build :: (Ord a) => Adj a -> GraphInfo a
build = entry where
  (!^) = (A.!); (!#) = (IntMap.!); (!@) = (Map.!)
  entry es
    | (sym, rev) <- nameAll (flatten es)
    , graph      <- accumArray (++) [] (0, Map.size sym - 1)
                   (map (bimap (sym !@) (map (sym !@))) es)
    = GI { giGraph = graph
         , giVertex = \v -> (rev !# v, map (rev !#) (graph !^ v))
         , giKey = (`Map.lookup` sym) }
  flatten ((x, ys) : zs) = x : ys ++ flatten zs
  flatten            []  = []
  nameAll r | ixs <- zip [0..] (nub r) =
    (  Map.fromList [(n, v) | (v, n) <- ixs] ,
    IntMap.fromList [(v, n) | (v, n) <- ixs] )
-- Using DP, count paths from 'you' to 'out', which are assumed to exist.
count :: Adj String -> Int
count es
  | gi <- build es, gr <- giGraph gi, ky <- giKey gi
  , rt <- reverse (topSort gr) = (! fromJust (ky "you")) $ V.create $ do
  let size = snd (bounds gr) + 1 -- 0-based
  vec <- M.new size
  forM_ rt $ \vtx -> do
    let (key, adj) = giVertex gi vtx
    cnt <- if key == "out"
      then pure 1 -- There's one path from 'out' to 'out'.
      else foldlM
      ( \a k -> case ky k of
        Just w  -> (a +) <$> M.read vec w
        Nothing -> error $ "count: missing key " ++ show k
      ) 0 adj
    M.write vec vtx cnt
  pure vec
readAdj :: ReadP (Adj0 String)
readAdj = do
  start <-  word <* string ": "
  neigh <- (word <* skipSpaces) `manyTill` eof -- 'many' causes ambiguity; manyTill is better
  pure (start, neigh) where word = munch1 isAlpha
main :: IO ()
main = interact $ show . count . map (fst . the . readP_to_S readAdj) . lines
