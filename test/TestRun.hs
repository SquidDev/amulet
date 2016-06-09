module Main (main) where

import TestInfer(inferTests)
import TestScope(scopeTests)
import Test(runTests)

main :: IO ()
main = runTests [inferTests, scopeTests]
