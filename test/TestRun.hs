module Main (main) where

import TestInfer(inferTests)
import Test(runTests)

main :: IO ()
main = runTests [inferTests]
