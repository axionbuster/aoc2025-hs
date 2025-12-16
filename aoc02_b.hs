{-# OPTIONS_GHC -Wall #-}

import qualified Data.Array            as A
import qualified Data.Array.Unboxed    as U
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Graph
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NEL
import           Data.Maybe
import           Debug.Trace
import           Text.Printf

-- | Count the number of repeated sequences between two ByteStrings interpreted as numbers.
repeats :: ByteString -> ByteString -> Integer
repeats = entry0 where
  entry0 bs1 bs2 = trace (printf "case of %v, %v" (B.unpack bs1) (B.unpack bs2)) $ entry bs1 bs2
  entry  bs1 bs2
    | len1 == len2 = trace0 $ entryN bs1 bs2 len1
    | otherwise    = -- len2 > len1, as inputs are well-formed
      trace0 $
      entryN bs1 (pad9s len1) len1 +
      sum [ entryN (pad10 n) (pad9s n) n
          | n <- [len1 + 1 .. len2 - 1]
          ] +
      entryN (pad10 len2) bs2 len2 where
    len1 = B.length bs1; len2 = B.length bs2
    pad9s n = B.replicate n '9'
    pad10 n = B.cons '1' (B.replicate (n - 1) '0')
  trace0 out = trace (printf " = %v" out) out
  entryN = (sumOnDivisors .) . entryNM
  entryNM bs1 bs2 chksz | h1 : t1 <- split bs1, h2 : t2 <- split bs2, chksz < n =
    let
    -- sums:
    -- if low is 123 and high is 234, then we sum 123123123...123 up to 234234234...234,
    -- NOT all the numbers in between, but only the numbers that have the same repeated pattern
    -- (e.g., 123123123..., 124124124..., ..., 234234234...).
    sums | low <= high = (low - high - 1) * (low + high) * (1 - 10 ^ n) `quot` (2 * (10 ^ chksz - 1))
         | otherwise   = 0
    low  | hiearchy (h1 : t1) == LT = h1 + 1 | otherwise = h1
    high | hiearchy (h2 : t2) == GT = h2 - 1 | otherwise = h2
    in trace (printf "entryNM %v %v %v = %v ...... low = %v, high = %v" chksz (B.unpack bs1) (B.unpack bs2) sums low high) sums
    | otherwise = 0 where
    -- Split the ByteString into chunks of size chksz.
    -- This division is exact since we only let chksz values that divide n.
    split = map (bread . B.take chksz) . takeWhile (not . B.null) . iterate (B.drop chksz)
    n     = B.length bs1
    -- Does the sequence of digits go like 55554..., 555...555, or 55556...?
    -- Ex: 5555491832 -> LT, 5555555555 -> EQ, 5555567890 -> GT.
    -- NOTE: we actually compare entire chunks, not individual digits.
    hiearchy (x : y : zs) = case compare x y of
      EQ -> hiearchy (y : zs)
      vd -> vd
    hiearchy _ = EQ
  bread = fst . fromJust . B.readInteger

divisors :: (Integral a) => a -> NonEmpty a
divisors n = 1 :| [d | d <- [2..n], n `rem` d == 0]

divisorsG
  :: (Integral label)
  => (label -> payload) -> label
  -> (Graph, Vertex -> (payload, label, [label]), label -> Maybe Vertex)
divisorsG f n = graphFromEdges        -- such a good function if you can tolerate its type
  [ (f k, k,   NEL.init $ divisors k) -- no self-loop
  |    k  <- NEL.toList $ divisors n
  ]

sumOnDivisors :: (Int -> Integer) -> Int -> Integer
sumOnDivisors f n = sum entry where
  (gr, vs, _) = divisorsG f n
  (lo, hi)    = U.bounds gr
  -- f assigns a sum to each term represented by a divisor of n.
  -- we want to get the sum for each divisor excluding the sums of its proper divisors.
  entry = A.array (lo, pred hi)                   -- exclude root, or else, get 0
    [ (i, f_i - sum (map (entry A.!) (gr A.! i))) -- own value; that is, minus children's values
    | i <- init $ reverse $ topSort gr            -- post-order traversal
    , let (f_i, _, _) = vs i                      -- assigned value
    ]

main :: IO ()
main = B.interact $ entry 0 . map pairs . B.split ',' where
  pairs = map B.strip . B.split '-'
  entry !c ((bs0 : bs1 : _) : bsss) = entry (c + repeats bs0 bs1) bsss
  entry !c  _                       = B.pack (show c)
