{-# LANGUAGE InstanceSigs #-}

module TicTacToe
  ( TPlayer (PX, PO),
    TField (X, O, NA),
    TBoard,
    TWin (WX, WO, WNULL, WND),
    TGameState,
    TGameStateChange,
    validMoves,
    initBoard,
    applyChange,
    isDone,
    simulateGame,
    applyMove,
    applyMoveH,
  )
where

import qualified GameState (Board (Board), GameState (GameState), GameStateChange (GameStateChange), GameStateOp (GameStateOp), GameStateOpHistory (GameStateOpHistory), field, move, stateBoard, statePlayer, runOp)
import RoseTree (Rose)
import qualified RoseTree (Rose (Node), elemsOnDepth, leaves)

data TPlayer = PX | PO deriving (Eq)

data TField = X | O | NA deriving (Eq)

data TBoard = TBoard (GameState.Board TField)

-- X won, O won, none won, still in progress
data TWin = WX | WO | WNULL | WND deriving (Eq, Show)

tSize :: Int
tSize = 3

type TGameState = GameState.GameState TField TPlayer

type TGameStateChange = GameState.GameStateChange TPlayer TField

instance Show TPlayer where
  show :: TPlayer -> String
  show PX = "X"
  show PO = "O"

instance Show TField where
  show :: TField -> String
  show X = "X"
  show O = "O"
  show NA = " "

validMoves :: TGameState -> [TGameStateChange]
validMoves (GameState.GameState b p) =
  let playerMove :: TField
      playerMove =
        if p == PX
          then X
          else O
      (GameState.Board t) = b
   in [ GameState.GameStateChange p (i, j) playerMove
        | i <- [0 .. (tSize - 1)],
          j <- [0 .. (tSize - 1)],
          (t !! i !! j) == NA
      ]

initBoard :: TGameState
initBoard = GameState.GameState (GameState.Board (replicate 3 (replicate 3 NA))) PX

applyChange :: TGameState -> TGameStateChange -> TGameState
applyChange s c =
  let (y, x) = GameState.field c
      (GameState.Board t) = GameState.stateBoard s
   in GameState.GameState
        ( GameState.Board
            [ [ if y == i && x == j
                  then GameState.move c
                  else t !! i !! j
                | j <- [0 .. (tSize - 1)]
              ]
              | i <- [0 .. (tSize - 1)]
            ]
        )
        ( if GameState.statePlayer s == PX
            then PO
            else PX
        )

isDone :: TGameState -> TWin
isDone s =
  let (GameState.Board t) = GameState.stateBoard s
      rows = [t !! i | i <- [0 .. (tSize - 1)]]
      cols =
        [ [(t !! j) !! i | j <- [0 .. (tSize - 1)]]
          | i <- [0 .. (tSize - 1)]
        ]
      diags =
        [ [t !! 0 !! 0, t !! 1 !! 1, t !! 2 !! 2],
          [t !! 0 !! 2, t !! 1 !! 1, t !! 2 !! 0]
        ]
      wonX =
        any (\row -> row == [X, X, X]) rows
          || any (\row -> row == [X, X, X]) cols
          || any (\row -> row == [X, X, X]) diags
      wonO =
        any (\row -> row == [O, O, O]) rows
          || any (\row -> row == [O, O, O]) cols
          || any (\row -> row == [O, O, O]) diags
      isWND = or [NA `elem` (t !! i) | i <- [0 .. (tSize - 1)]]
   in if wonX
        then WX
        else
          if wonO
            then WO
            else
              if isWND
                then WND
                else WNULL

simulateGame :: TGameState -> Rose TGameState
simulateGame s =
  let done = isDone s
      possibleMoves = validMoves s
      possibleStates = [simulateGame $ applyChange s m | m <- possibleMoves]
   in if done /= WND
        then RoseTree.Node s []
        else RoseTree.Node s possibleStates

applyMove :: (Int, Int) -> GameState.GameStateOp TGameState Bool
applyMove (x, y) = GameState.GameStateOp $ \s ->
  let p :: TPlayer
      p = GameState.statePlayer s
      f :: TField
      f = if p == PX then X else O
      c :: TGameStateChange
      c = GameState.GameStateChange p (x, y) f
      s' = applyChange s c
      v :: Bool
      v = isDone s' /= WND
   in if x > (tSize - 1) || x < 0 || y > (tSize - 1) || y < 0
        then error "Illegal argument"
        else (v, s')


applyMoveH :: (Int, Int) -> GameState.GameStateOpHistory TGameState Bool
applyMoveH (x, y) = GameState.GameStateOpHistory $ \s ->
  let h :: TGameState
      h = head s
      p :: TPlayer
      p = GameState.statePlayer h
      f :: TField
      f = if p == PX then X else O
      c :: TGameStateChange
      c = GameState.GameStateChange p (x, y) f
      s' = applyChange h c
      v :: Bool
      v = isDone s' /= WND
   in if x > (tSize - 1) || x < 0 || y > (tSize - 1) || y < 0
        then error "Illegal argument"
        else (v, s' : s)


b :: TGameState
b = initBoard
c :: Rose TGameState
c = simulateGame b
d :: [TGameState]
d =  RoseTree.elemsOnDepth c 5