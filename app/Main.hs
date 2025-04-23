module Main (
    main
  ) where

import Eval
import Interpret
import qualified System.Environment as S

main :: IO ()
main = do 
  [fp] <- S.getArgs
  run fp