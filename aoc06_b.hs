{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}
import           Data.List

-- | Solve the AoC 2024 Day 6 Part B problem
solve :: [String] -> Integer
solve ((decode1 -> Just (op, nstr1)) : ls)
  | (nstrs, _ : rest) <- break (all (== ' ')) ls =
  op (read nstr1 : (nstrs >>= map read . words)) + solve rest
solve _ = 0

-- | Decode problem header
decode1 :: (Foldable t, Num a) => String -> Maybe (t a -> a, String)
decode1 str = case last str of
  '+' -> Just (sum    , init str)
  '*' -> Just (product, init str)
  _   -> Nothing

main :: IO ()
main = interact (show . solve . transpose . map pad . lines) where
  pad = (++ replicate 10 ' ') -- ensure enough trailing spaces
