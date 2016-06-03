module Test
  (
    Result(..),
    Test(..),
    TestRun, runTests
  ) where

import Control.Monad.State
import Text.Printf

import System.CPUTime
import System.Environment
import System.Exit

-- | The result of a test
data Result
   -- | A passed test
   = Pass
   -- | A failed test
   | Fail String
   -- | An error occured
   | Error String

type TestRun = IO Result

-- | The test to be run
data Test
  = Test String TestRun
  | Group String [Test]

data TestState = TestState { results :: [(String, Result)], passCount :: Int, failCount :: Int, errorCount :: Int }

initState :: TestState
initState = TestState [] 0 0 0

runTest :: Test -> StateT TestState IO ()
runTest (Test name runner) = do
  result <- liftIO runner
  state <- get
  liftIO $ putStr $ resultDecoration result
  put $ increment result (state { results = (name, result) : results state })
  where
    resultDecoration Pass = colour "●" 2
    resultDecoration (Fail _) = colour "◼" 1
    resultDecoration (Error _) = colour "✱" 5

    increment Pass state = state { passCount = passCount state + 1 }
    increment (Fail _) state = state { failCount = failCount state + 1 }
    increment (Error _) state = state { errorCount = errorCount state + 1 }

runTest (Group name tests) = mapM_ (runTest . prefix name) tests
  where prefix pre (Test name runner) = Test (pre ++ " " ++ name) runner
        prefix pre (Group name tests) = Group (pre ++ " " ++ name) tests

printTests :: [(String, Result)] -> Bool -> IO ()
printTests [] _ = return ()
printTests ((_, Pass):remaining) False = printTests remaining False
printTests ((name, result):remaining) all = do
  let (str, msg) = extractResult result
  putStrLn $ str ++ " " ++ name

  case msg of
    Just msg -> putStrLn msg
    Nothing -> return ()
  printTests remaining all
  where
    extractResult Pass = (colour "→ " 2, Nothing)
    extractResult (Fail msg) = (colour "→ " 1, Just msg)
    extractResult (Error msg) = (colour "→ " 5, Just msg)

colour :: String -> Integer -> String
colour a n = clr ++ a ++ "\x1b[0m"
  where clr = "\x1b[1;3" ++ show n ++ "m"

runTests :: [Test] -> IO ()
runTests tests = do
  args <- getArgs
  startTime <- getCPUTime
  TestState result pass fail error <- execStateT (mapM_ runTest tests) initState
  endTime <- getCPUTime
  let diffTime = fromIntegral (endTime - startTime) / (10^12)
  putStrLn ""
  printTests (reverse result) (elem "--passsed" args || elem "-p" args)
  putStrLn $ printf "Ran %d tests in %0.3f seconds" (pass + fail + error) (diffTime :: Double)
  putStrLn $ printf "Passed: %d, Failed: %d, Errored: %d" pass fail error
  if fail > 0 || error > 0 then exitFailure else exitSuccess
