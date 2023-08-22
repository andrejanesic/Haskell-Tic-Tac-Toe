module Main (main) where

import Control.Monad (when)
import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), runTestTT)
import qualified TestGameState (tests)
import qualified TestRoseTree (tests)
import qualified TestTicTacToe (tests)

main :: IO ()
main = do
  -- TestRoseTree
  result <- runTestTT TestRoseTree.tests
  when (failures result > 0) Exit.exitFailure

  -- TestGameState
  result <- runTestTT TestGameState.tests
  when (failures result > 0) Exit.exitFailure

  -- TestTicTacToe
  result <- runTestTT TestTicTacToe.tests
  when (failures result > 0) Exit.exitFailure