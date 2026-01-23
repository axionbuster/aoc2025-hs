{-# OPTIONS_GHC -Wall #-}
import           Data.Bits
import           Data.Foldable
import           Data.Function
import           Data.Monoid
import           Debug.Trace
import           Text.Printf
import           Text.Read
data P = P !Int !Int deriving (Eq, Ord) -- x, y
data E = E !P   !P   deriving (Eq, Ord) -- edge from P to P
eMinY, eMaxY :: E -> Int
eMinY (E (P _ y0) (P _ y1)) = min y0 y1
eMaxY (E (P _ y0) (P _ y1)) = max y0 y1
eMinX, eMaxX :: E -> Int
eMinX (E (P x0 _) (P x1 _)) = min x0 x1
eMaxX (E (P x0 _) (P x1 _)) = max x0 x1
instance Show P where show (P x y) = printf "(%5v,%5v)" x y
instance Read P where readPrec = do x <- readPrec <* get; P x <$> readPrec
formatArgDefault :: (Show a) => a -> FieldFormatter
formatArgDefault x FieldFormat {fmtChar = 'v'} = shows x
formatArgDefault _ FieldFormat {fmtChar}       = errorBadFormat fmtChar
{-# INLINE formatArgDefault #-}
instance PrintfArg P where formatArg = formatArgDefault
instance Show E where
  showsPrec d (E p0 p1) = t '[' . s p0 . u " -> " . s p1 . t ']' where
    t = showChar; s = showsPrec d; u = showString
instance PrintfArg E where formatArg = formatArgDefault
-- Check if a right-facing ray intersects a segment except possibly at the endpoints.
_eMeetsRightRay :: P -> E -> Bool
_eMeetsRightRay (P rx ry) = entry where
  entry e -- Break up so this frequently-called function is inlined well when the P is known.
    | ri rx < fi (eMinX e) = fi (eMinY e) < ri ry && ri ry < fi (eMaxY e)
    | otherwise            = False
  fi = fromIntegral @Int @Double
  ri = (+ 0.5) . fi -- cast to half-integer to avoid edge cases.
{-# INLINE _eMeetsRightRay #-}
-- Like eMeetsRightRay but returns Ordering that indicates orientation.
--   GT = counterclockwise (up)
--   EQ = no intersection (edge collinear with ray or ray misses edge)
--   LT = clockwise (down)
_eMeetsRightRayO :: P -> E -> Ordering
_eMeetsRightRayO (P rx ry) = entry where
  entry e@(E (P _ ey0) (P _ ey1)) -- Need to know edge direction for orientation.
    | ri rx < fi (eMinX e) = case cp ry ey0 of
      GT -> case cp ry ey1 of
        LT -> LT -- intersection going down.
        _  -> EQ -- at or below (more positive) edge, no intersection.
      LT -> case cp ry ey1 of
        GT -> GT -- intersection going up.
        _  -> EQ -- at or above (less positive) edge, no intersection.
      _  -> EQ -- at edge endpoint, no intersection.
    | otherwise = EQ
  fi = fromIntegral @Int @Double
  ri = (+ 0.5) . fi -- cast to half-integer to avoid edge cases.
  cp a b = ri a `compare` fi b
{-# INLINE _eMeetsRightRayO #-}
-- Check if edge makes an intersection with the proper inside of the box.
eStabsBox :: (P, P) -> E -> Bool
eStabsBox box_ = entry where
  entry e
    | horizontal e = hor e' box_
    | otherwise    = ver e' where
    e' = E (P (eMinX e) (eMinY e)) (P (eMaxX e) (eMaxY e))
  horizontal (E (P _ y0) (P _ y1)) = y0 == y1
  ver (E (P x0 y0) (P x1 y1))
    | (P bx0 by0, P bx1 by1) <- box_ =
      -- Flip then use hor
      hor (E (P y0 x0) (P y1 x1)) (P by0 bx0, P by1 bx1)
  hor (E (P x0 y) (P x1 _)) (P bx0 by0, P bx1 by1)
    | y <= by0 || y >= by1 = False
    | otherwise = min x0 x1 < bx1 && max x0 x1 > bx0
{-# INLINE eStabsBox #-}
normalizeBox :: (P, P) -> (P, P)
normalizeBox (P x0 y0, P x1 y1) = (P (min x0 x1) (min y0 y1), P (max x0 x1) (max y0 y1))
-- Find a good integer midpoint for use with even-odd rule.
boxMidpoint :: (P, P) -> P
boxMidpoint (P cr0x cr0y, P cr1x cr1y) = P (avg cr0x cr1x) (avg cr0y cr1y) where
  avg x y = (x + y) `div` 2
-- Even-odd rule by exhaustive ray casting to the right from box midpoint.
-- Box must be normalized.
_evenOdd :: (Foldable f) => f E -> (P, P) -> Bool
_evenOdd pts box = getXor (foldMap' (Xor . crossing) pts) where
  crossing = _eMeetsRightRay $ boxMidpoint box
-- Check that the box is inside the polygon by winding number rule,
-- where a point is declared "inside" if the winding number is nonzero.
_winding :: (Foldable f) => f E -> (P, P) -> Bool
_winding pts box = getSum (foldMap' (Sum . w) pts) /= 0 where
  w = subtract 1 . fromEnum . _eMeetsRightRayO (boxMidpoint box)
-- POST HOC reflection: _evenOdd and _winding give the same answer for the
-- input, so it appears this is a simple polygon. Thus either test suffices.
boxIsThin :: (P, P) -> Bool
boxIsThin (P x0 y0, P x1 y1) = x0 == x1 || y0 == y1
scroll :: [a] -> [(a, a)]
scroll []       = []
scroll (x : xs) = map (x,) xs ++ scroll xs
-- Expected cubic time and quadratic space solution.
main :: IO ()
main = interact $ show . entry . map read . lines where
  entry pts = area $ traceRect $ maximumBy (compare `on` area) (dummy : traceBoxes boxes) where
    dummy = (P 0 0, P 0 0) -- area 1, exists to handle no boxes case.
    edges = zipWith E pts (drop 1 $ cycle pts)
    boxes =
      [ b | (p1, p2) <- scroll pts
      , let b = (p1, p2); b' = normalizeBox b, boxIsThin b' || _winding verticals b'
      , not $ any (eStabsBox b') edges
      ]
    area = area_entry . normalizeBox where
      area_entry (P x0 y0, P x1 y1) = (x1 - x0 + 1) * (y1 - y0 + 1)
    verticals = [e | e@(E (P x0 _) (P x1 _)) <- edges, x0 == x1]
    traceRect = traceShowId
    traceBoxes = traceWith $ \bs -> printf "comparing %v boxes" (length bs)
