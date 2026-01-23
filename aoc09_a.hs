{-# OPTIONS_GHC -Wall #-}
import           Control.Monad
import           Text.Read
data Point = P Integer Integer
instance Read Point where
  readPrec = do
    x <- readPrec
    c <- get; guard (c == ',')
    P x <$> readPrec
naive :: [Point] -> Integer
naive pts = maximum
  [ abs dx * abs dy
  | P x1 x2 <- pts, P y1 y2 <- pts
  , let dx = x1 - y1 + 1; dy = x2 - y2 + 1
  ]
main :: IO ()
main = interact $ show . naive . map read . lines
-- | Debug instance
instance Show Point where
  showsPrec d (P x y) = t '(' . s x . t ',' . s y . t ')' where
    t = showChar; s = showsPrec d
