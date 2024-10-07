module Main (main) where

import Data.Number.Flint.Arb

main :: IO ()
main = do
    x <- arb_new
    arb_drop x
    print "done!"
