{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}
-- vector
import           Control.Monad
import           Control.Monad.ST
import           Data.Function
import           Data.Ord
import qualified Data.Vector.Algorithms.Heap as Heapsort
import qualified Data.Vector.Strict          as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as W
import           Debug.Trace
import           Text.Printf
type Coord = Int
type Index = Int
type Cardi = Int -- Please excuse the funny name.
data P = P !Coord !Coord !Coord
sqdist :: P -> P -> Coord
sqdist (P x1 y1 z1) (P x2 y2 z2) = dx * dx + dy * dy + dz * dz where
  dx = x1 - x2; dy = y1 - y2; dz = z1 - z2
-- Basic disjoint set data structure with path compression and union by count.
type Disj s = U.MVector s Node
-- Allow using an unboxed vector to minimize both memory and indirections,
-- not to mention lessening the stress on the garbage collector.
type Node = Int
discriminateInt :: Int -> Either Int Int -- left for negative (abs val), right for non-negative
discriminateInt n | n < 0 = Left (negate n) | otherwise = Right n
pattern Root :: Cardi -> Node -- Since size is always positive, we can negate it to mark roots.
pattern Root c <- (discriminateInt -> Left c) where Root c = negate c
pattern Link :: Index -> Node -- Indices are always non-negative.
pattern Link p <- (discriminateInt -> Right p) where Link p = p
{-# COMPLETE Root, Link #-}
{-# INLINE discriminateInt #-}
{-# INLINE Root #-}
{-# INLINE Link #-}
djInit :: Cardi -> ST s (Disj s)
djInit = (`W.replicate` Root 1)
djFind :: Disj s -> Index -> ST s Index
djFind dj i = W.read dj i >>= \case
  -- Simple path compression, instead of the complex ones that do halving or splitting.
  Root _ -> pure i
  Link p -> do r <- djFind dj p; r <$ W.write dj i (Link r)
-- Returns True if union was performed, False if already in same set.
djUnion :: Disj s -> Index -> Index -> ST s Bool
djUnion dj i j = do
  ri <- djFind dj i; rj <- djFind dj j
  if ri /= rj
    then True <$ do
    let w = W.write dj
    xi <- W.read dj ri; xj <- W.read dj rj
    case (xi, xj) of
      (Root ci, Root cj)
        | ci < cj   -> w ri (Link rj) >> w rj (Root (ci + cj))
        | otherwise -> w rj (Link ri) >> w ri (Root (cj + ci))
      _ -> error "djUnion: djFind returned a non-root node"
    else pure False
-- Aggregate points into clusters by proximity between the points, but use
-- a clustering count to limit the number of unions performed.
solve :: Cardi -> [P] -> Cardi
solve = entry where
  entry steps ps = runST $ do
    let vs = V.map (uncurry ((,) `on` fst)) $ V.take steps $
             V.modify (Heapsort.sortBy distcmp) $
             do (i, p) <- qs; (j, q) <- qs; ((i, p), (j, q)) <$ guard (i < j)
        qs = V.indexed $ V.fromList ps
    dj <- djInit (length ps)
    V.mapM_ (uncurry (djUnion dj)) vs >> report dj
  report dj = U.product . U.take 3 . traceShowId .
    U.modify (Heapsort.sortBy (comparing Down)) .
    U.mapMaybe (\case Root s -> Just s; _ -> Nothing) <$>
    U.unsafeFreeze dj
  distcmp ((i, p), (j, q)) ((k, r), (l, s)) =
    comparing (uncurry sqdist) (p, q) (r, s) <>
    compare i k <> compare j l -- tie-breakers
-- Adjust the step counts per scenario:
--  Example: 10   connections.
--  Real   : 1000 connections.
main :: IO ()
main = interact $ show . solve connections . map readP . lines where
  readP s
    | [x, y, z] <- map read $ split s = P x y z
    | otherwise = error "bad point"
  split str = case break (== ',') str of
    (tok, "")       -> [tok]
    (tok, _ : rest) ->  tok : split rest
  connections =
    traceWith (printf "configured to track %v connections")
    _realC
  _exampleC :: Cardi = 10; _realC :: Cardi = 1000
