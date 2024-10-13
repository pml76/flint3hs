module Main (main) where

import Data.Number.Flint.Arb
import Data.Number.Flint.FlintVariable (FlintVariable (withFlintVariable))

main :: IO ()
main = do
    a <- arbCreate
    withFlintVariable arbZero a
    print "done!"
