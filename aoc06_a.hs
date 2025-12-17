{-# OPTIONS_GHC -Wall #-}
import           Data.List

main :: IO ()
main = interact (show @Integer . interpret . map reverse . transpose . map words . lines) where
  interpret (("+" : terms  ) : problems) = sum     (map read terms  ) + interpret problems
  interpret (("*" : factors) : problems) = product (map read factors) + interpret problems
  interpret []                           = 0
  interpret _                            = error "invalid input"
