{-# LANGUAGE InstanceSigs #-}

module GameState
  ( GameState (GameState),
    Board (Board),
    stateBoard,
    statePlayer,
    GameStateChange (GameStateChange),
    owner,
    field,
    move,
    GameStateOp (GameStateOp),
    runOp,
    GameStateOpHistory (GameStateOpHistory),
    runOpHistory,
  )
where

import Data.Char (isSpace)

-- https://prnt.sc/L1wutl7W11ay

newtype Board tField = Board [[tField]] deriving (Eq)

instance (Show tField) => Show (Board tField) where
  show :: (Board tField) -> String
  show (Board b) =  foldl f "" b
    where
      f :: (Show tField) => String -> [tField] -> String
      f acc xs =
        acc
          ++ foldl (\a x -> a ++ show x ++ "|") "|" xs
          ++ "\n"

-- Board represents the state of the board and player represents the next
-- player about to make a GameStateChange.
data GameState tField tPlayer = GameState
  { stateBoard :: Board tField,
    statePlayer :: tPlayer
  }
  deriving (Eq)

instance (Show tField, Show tPlayer) => Show (GameState tField tPlayer) where
  show :: (Show tField, Show tPlayer) => GameState tField tPlayer -> String
  show (GameState sb sp) = "Board state:\n" ++ show sb ++ "\nNext player: " ++ show sp

-- The owner of the move makes a move on the field.
data GameStateChange tPlayer tField = GameStateChange
  { owner :: tPlayer,
    field :: (Int, Int),
    move :: tField
  }
  deriving (Eq, Show)

newtype GameStateOp s a = GameStateOp
  { runOp :: s -> (a, s)
  }

instance Functor (GameStateOp s) where
  fmap :: (a -> b) -> GameStateOp s a -> GameStateOp s b
  fmap f (GameStateOp g) = GameStateOp $ \s0 ->
    let (a, s1) = g s0
     in (f a, s1)

instance Applicative (GameStateOp s) where
  pure :: a -> GameStateOp s a
  pure a = GameStateOp (\s -> (a, s))
  (<*>) :: GameStateOp s (a -> b) -> GameStateOp s a -> GameStateOp s b
  GameStateOp f <*> GameStateOp g = GameStateOp $ \s0 ->
    let (f', s1) = f s0
        (a, s2) = g s1
     in (f' a, s2)

instance Monad (GameStateOp s) where
  return :: a -> GameStateOp s a
  return a = GameStateOp (\s -> (a, s))
  (>>=) :: GameStateOp s a -> (a -> GameStateOp s b) -> GameStateOp s b
  GameStateOp f >>= g = GameStateOp $ \s0 ->
    let (a, s1) = f s0
        h = g a
     in runOp h s1

newtype GameStateOpHistory s a = GameStateOpHistory
  { runOpHistory :: [s] -> (a, [s])
  }

instance Functor (GameStateOpHistory s) where
  fmap :: (a -> b) -> GameStateOpHistory s a -> GameStateOpHistory s b
  fmap f (GameStateOpHistory g) = GameStateOpHistory $ \s0 ->
    let (a, s1) = g s0
     in (f a, s1)

instance Applicative (GameStateOpHistory s) where
  pure :: a -> GameStateOpHistory s a
  pure a = GameStateOpHistory (\s -> (a, s))
  (<*>) :: GameStateOpHistory s (a -> b) -> GameStateOpHistory s a -> GameStateOpHistory s b
  GameStateOpHistory f <*> GameStateOpHistory g = GameStateOpHistory $ \s0 ->
    let (f', s1) = f s0
        (a, s2) = g s1
     in (f' a, s2)

instance Monad (GameStateOpHistory s) where
  return :: a -> GameStateOpHistory s a
  return a = GameStateOpHistory (\s -> (a, s))
  (>>=) :: GameStateOpHistory s a -> (a -> GameStateOpHistory s b) -> GameStateOpHistory s b
  GameStateOpHistory f >>= g = GameStateOpHistory $ \s0 ->
    let (a, s1) = f s0
        h = g a
     in runOpHistory h s1