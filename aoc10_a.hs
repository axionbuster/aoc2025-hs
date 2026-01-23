{-# OPTIONS_GHC -Wall #-}
import           Data.Bits
import           Data.Char                    (isDigit)
import           Data.Function
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           Text.ParserCombinators.ReadP
import           Text.Printf
import           Text.Read                    (Read (..), lift)
data Problem -- Ignore the joltage part at the end.
  = Problem
  { pTarget  ::  !Integer  -- ^ Target value (carryless add/XOR)
  , pVectors :: ![Integer] -- ^ List of bit vectors
  }
readProblem :: ReadP Problem
readProblem = entry where
  entry = do
    t <- inBrackets target                      -- '#' for 1, '.' for 0, binary
    v <- map (sum . map (bit . read)) <$> many' -- e.g., "(2,6,10)" to set bits 2, 6, and 10
      (skipSpaces >> inParentheses (munch1 isDigit `sepBy1` char ','))
    let go = eof <++ (get >> go) in go -- Consume all remaining input.
    pure $ Problem t v
  -- Parse sequences of '#' and '.' into a number in little-endian order.
  target = chainr tdigit (pure $ \d a -> d + 2 * a) 0
  tdigit = (char '.' $> 0) <++ (char '#' $> 1)
  inBrackets    = between (char '[') (char ']')
  inParentheses = between (char '(') (char ')')
  -- Get around the inefficient ReadP 'many' combinator for zero-or-more when we *know*
  -- we want to parse as many as possible or else fail. This helps resolve
  -- ambiguities as well.
  many' p = some' p <++ pure []; some' p = liftA2 (:) p (many' p)
instance Read Problem where readPrec = lift readProblem
instance Show Problem where -- Not symmetric with Read; shows problem in binary notation for target and vectors.
  showsPrec _ Problem {pTarget, pVectors} = showParen True $
    bracket (showBinary pTarget) . str " <- {" .
    apply (intersperse (str ", ") (map showBinary pVectors)) .
    chr '}'
    where -- traditional binary notation
      showBinary 0 = showChar '0'
      showBinary s = apply . reverse . go $ s where
        go 0 = []
        go n | (q, r) <- n `quotRem` 2 = chr (if r == 0 then '0' else '1') : go q
      bracket x = chr '[' . x . chr ']'
      apply = foldr (.) id
      str   = showString
      chr   = showChar
instance PrintfArg Problem where
  formatArg p FieldFormat {fmtChar = 'v'} = shows p
  formatArg _ FieldFormat {fmtChar}       = errorBadFormat fmtChar
-- Find the least number of bit vectors that must be carryless-added to
-- make the target from 0, if the heuristic determines that the problem
-- is small enough to solve by exhaustive search.
brute :: Problem -> Maybe Int
brute = entry where
  thr = 16 -- Give up on problems with this many or more vectors (never triggered).
  entry Problem {pTarget, pVectors}
    | null          pVectors           = if pTarget == 0 then Just 0 else Nothing
    | compareLength pVectors thr == LT = Just $ work pTarget pVectors
    | otherwise                        = Nothing -- Too large for naive search.
  work t vs = fst $ minimumBy (compare `on` fst)
    [ (length vs', vs')
    | vs' <- subsequences vs, foldl' xor 0 vs' == t
    ]
main :: IO ()
main = interact $ show . sum . mapMaybe (traceMaybe . brute . read) . lines where
  traceMaybe Nothing = trace "At least one Nothing found." Nothing
  traceMaybe x       = x
