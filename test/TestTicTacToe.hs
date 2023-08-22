module TestTicTacToe (tests) where

import Control.Exception (ErrorCall (ErrorCall), catch)
import Data.Char (chr)
import GHC.IO.Exception (assertError)
import GameState (Board (Board), GameState (GameState), GameStateChange (GameStateChange))
import RoseTree (Rose (Node), height, leaves, leavesCount, nodesOnDepth)
import Test.HUnit
import TicTacToe (TField (NA, O, X), TGameState, TGameStateChange, TPlayer (PO, PX), TWin (WND, WNULL, WO, WX), initBoard, isDone, simulateGame, validMoves)

testValidMoves :: Test
testValidMoves = TestCase $ do
  assertEqual
    "Returns valid moves"
    [ GameState.GameStateChange PX (0, 2) X,
      GameState.GameStateChange PX (2, 2) X
    ]
    ( validMoves
        ( GameState.GameState
            ( GameState.Board
                [ [X, O, NA],
                  [O, O, X],
                  [X, X, NA]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns valid moves"
    [ GameState.GameStateChange PO (0, 1) O,
      GameState.GameStateChange PO (1, 0) O,
      GameState.GameStateChange PO (1, 1) O,
      GameState.GameStateChange PO (2, 1) O,
      GameState.GameStateChange PO (2, 2) O
    ]
    ( validMoves
        ( GameState.GameState
            ( GameState.Board
                [ [X, NA, O],
                  [NA, NA, X],
                  [X, NA, NA]
                ]
            )
            PO
        )
    )
  assertEqual
    "Returns valid moves"
    [ GameState.GameStateChange PX (0, 1) X,
      GameState.GameStateChange PX (0, 2) X,
      GameState.GameStateChange PX (1, 0) X,
      GameState.GameStateChange PX (1, 2) X,
      GameState.GameStateChange PX (2, 0) X,
      GameState.GameStateChange PX (2, 1) X,
      GameState.GameStateChange PX (2, 2) X
    ]
    ( validMoves
        ( GameState.GameState
            ( GameState.Board
                [ [O, NA, NA],
                  [NA, X, NA],
                  [NA, NA, NA]
                ]
            )
            PX
        )
    )

testIsDone :: Test
testIsDone = TestCase $ do
  assertEqual
    "Returns not done"
    WND
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [X, X, NA],
                  [O, NA, NA],
                  [X, X, NA]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns not done"
    WND
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [X, O, NA],
                  [NA, X, O],
                  [X, X, NA]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns not done"
    WND
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [NA, NA, X],
                  [NA, NA, O],
                  [X, X, O]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns win X"
    WX
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [X, X, X],
                  [O, NA, O],
                  [X, NA, O]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns win X"
    WX
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [X, O, X],
                  [O, X, O],
                  [X, X, O]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns win X"
    WX
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [X, O, X],
                  [O, X, O],
                  [X, O, X]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns win O"
    WO
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [O, X, X],
                  [O, O, O],
                  [X, X, O]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns win O"
    WO
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [O, X, O],
                  [X, O, X],
                  [O, X, O]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns win O"
    WO
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [O, X, X],
                  [X, O, X],
                  [O, O, O]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns nobody wins"
    WNULL
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [O, X, X],
                  [X, O, O],
                  [O, X, X]
                ]
            )
            PX
        )
    )
  assertEqual
    "Returns nobody wins"
    WNULL
    ( isDone
        ( GameState.GameState
            ( GameState.Board
                [ [X, O, X],
                  [X, O, O],
                  [O, X, X]
                ]
            )
            PX
        )
    )

testSimulateGame :: Test
testSimulateGame = TestCase $ do
  let initialState =
        GameState.GameState
          ( GameState.Board
              [ [X, O, O],
                [X, X, NA],
                [O, X, NA]
              ]
          )
          PO
      res = simulateGame initialState
      exp =
        Node
          initialState
          [ Node
              ( GameState.GameState
                  ( GameState.Board
                      [ [X, O, O],
                        [X, X, O],
                        [O, X, NA]
                      ]
                  )
                  PX
              )
              [ Node
                  ( GameState.GameState
                      ( GameState.Board
                          [ [X, O, O],
                            [X, X, O],
                            [O, X, X]
                          ]
                      )
                      PO
                  )
                  []
              ],
            Node
              ( GameState.GameState
                  ( GameState.Board
                      [ [X, O, O],
                        [X, X, NA],
                        [O, X, O]
                      ]
                  )
                  PX
              )
              [ Node
                  ( GameState.GameState
                      ( GameState.Board
                          [ [X, O, O],
                            [X, X, X],
                            [O, X, O]
                          ]
                      )
                      PO
                  )
                  []
              ]
          ]
  assertEqual
    "Returns correct game states"
    exp
    res
  let initialState =
        GameState.GameState
          ( GameState.Board
              [ [NA, O, NA],
                [NA, X, O],
                [O, X, NA]
              ]
          )
          PX
      res = simulateGame initialState
  assertEqual
    "Returns leaves with 0 size children"
    True
    ( all
        (\(Node r c) -> null c)
        $ nodesOnDepth res (height res)
    )
  assertEqual
    "Returns exact number of combinations"
    (4 * 3 * 2)
    (leavesCount res)
  assertEqual
    "Returns correct tree depth"
    4
    (height res)

tests :: Test
tests =
  TestList
    [ TestLabel "testIsDone" testIsDone,
      TestLabel "testValidMoves" testValidMoves,
      TestLabel "testSimulateGame" testSimulateGame
    ]
