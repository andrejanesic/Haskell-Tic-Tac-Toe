module TestGameState (tests) where

import Control.Exception (ErrorCall (ErrorCall), catch)
import Data.Char (chr)
import GHC.IO.Exception (assertError)
import GameState
import RoseTree (Rose (Node), children, elemsOnDepth, height, leaves, leavesCount, size, value)
import Test.HUnit

testGameStateOp :: Test
testGameStateOp =
  let initialState = "321"
      gso1 = (GameStateOp $ \s0 -> (0, s0 ++ "a"))
      gso2 = (GameStateOp $ \s0 -> (0, s0 ++ "b"))
      gso3 = (GameStateOp $ \s0 -> (0, s0 ++ "c"))
      (v, s) =
        runOp gso3 . snd $
          runOp gso2 . snd $
            runOp gso1 initialState
   in TestCase $ assertEqual "Returns correct final state" "321abc" s

testGameStateOpFunctor :: Test
testGameStateOpFunctor =
  let gso1 = (+ 5) <$> (GameStateOp $ \s0 -> (0 :: Int, s0)) :: GameStateOp String Int
      gso2 = (GameStateOp $ \s0 -> (5 :: Int, s0))
      s0 = "init"
      (v1, s1) = runOp gso1 s0
      (v2, s2) = runOp gso2 s0
   in TestCase $
        do
          assertEqual
            "Returns correct value for f <$> M"
            v2
            v1
          assertEqual
            "Returns correct state for f <$> M"
            s2
            s1

testGameStateOpFunctorLaws :: Test
testGameStateOpFunctorLaws =
  let f = (* 15)
      g = (+ 7)
      gso = (GameStateOp $ \s0 -> (v0, s0))
      v0 = 1
      s0 = "init"
   in TestCase $
        do
          assertEqual
            "Returns identical value for id <$> M"
            v0
            (fst $ runOp (id <$> gso) s0)
          assertEqual
            "Returns identical state for f <$> M"
            s0
            (snd $ runOp (id <$> gso) s0)
          assertEqual
            "Returns same value for f.g <$> M and f <$> (g <$> M)"
            True
            ( (fst $ runOp (f . g <$> gso) s0)
                == (fst $ runOp (f <$> (g <$> gso)) s0)
            )
          assertEqual
            "Returns correct value for f.g <$> M"
            120
            (fst $ runOp (f . g <$> gso) s0)
          assertEqual
            "Returns same state for f.g <$> M and f <$> (g <$> M)"
            True
            ( (snd $ runOp (f . g <$> gso) s0)
                == (snd $ runOp (f <$> (g <$> gso)) s0)
            )
          assertEqual
            "Returns correct state for f.g <$> M"
            "init"
            (snd $ runOp (f . g <$> gso) s0)

testGameStateOpApplicative :: Test
testGameStateOpApplicative =
  let f = GameStateOp (\s -> ((* 11), s ++ "!"))
      g = GameStateOp (\s -> (13, s))
      s0 = "initial"
      r = f <*> g
      (v, s) = runOp r s0
   in TestCase $
        do
          assertEqual
            "Returns correct value for f <*> g"
            143
            v
          assertEqual
            "Returns correct state for f <*> g"
            "initial!"
            s

testGameStateOpApplicativeLaws :: Test
testGameStateOpApplicativeLaws =
  let f = (* 11) :: Int -> Int
      g = GameStateOp (\s -> (13, s)) :: GameStateOp String Int
      u = GameStateOp (\s -> ((* 5), s)) :: GameStateOp String (Int -> Int)
      v = GameStateOp (\s -> ((* 18), s)) :: GameStateOp String (Int -> Int)
      w = GameStateOp (\s -> (1, s))
      h = 7 :: Int
      s0 = "initial"
      execValue g s = fst $ runOp g s
      execState g s = snd $ runOp g s
   in TestCase $
        do
          assertEqual
            "Value of (pure f) <*> g equals value of f <$> g"
            (execValue (pure f <*> g) s0)
            (execValue (f <$> g) s0)
          assertEqual
            "State of (pure f) <*> g equals state of f <$> g"
            (execState (pure f <*> g) s0)
            (execState (f <$> g) s0)
          assertEqual
            "Value of pure (.) <*> u <*> v <*> w equals value of u <*> (v <*> w)"
            (execValue (pure (.) <*> u <*> v <*> w) s0)
            (execValue (u <*> (v <*> w)) s0)
          assertEqual
            "State of pure (.) <*> u <*> v <*> w equals state of u <*> (v <*> w)"
            (execState (pure (.) <*> u <*> v <*> w) s0)
            (execState (u <*> (v <*> w)) s0)
          assertEqual
            "Value of pure f <*> pure x equals value of pure (f x)"
            (execValue (pure f <*> pure h) s0)
            (execValue (pure (f h)) s0)
          assertEqual
            "State of pure f <*> pure x equals state of pure (f x)"
            (execState (pure f <*> pure h) s0)
            (execState (pure (f h)) s0)
          assertEqual
            "Value of pure u <*> pure y equals value of pure ($ y) <*> u"
            (execValue (v <*> pure h) s0)
            (execValue (pure ($ h) <*> v) s0)
          assertEqual
            "State of pure u <*> pure y equals state of pure ($ y) <*> u"
            (execState (v <*> pure h) s0)
            (execState (pure ($ h) <*> v) s0)

testGameStateOpMonad :: Test
testGameStateOpMonad =
  let f = GameStateOp (\s -> (2, s * 11)) :: GameStateOp Int Int
      g x = (GameStateOp $ \s -> (x + 5, s * 13))
      h x = (GameStateOp $ \s -> (x + 17, s * 23))
      s0 = 8
   in TestCase $ do
        assertEqual
          "Returns correct value on bind"
          (2 + 5)
          (fst $ runOp (f >>= g) s0)
        assertEqual
          "Returns correct state on bind"
          (8 * 11 * 13)
          (snd $ runOp (f >>= g) s0)
        assertEqual
          "Returns correct value on multiple binds"
          (2 + 5 + 17)
          (fst $ runOp (f >>= g >>= h) s0)
        assertEqual
          "Returns correct state on multiple binds"
          (8 * 11 * 13 * 23)
          (snd $ runOp (f >>= g >>= h) s0)

testGameStateOpMonadLaws :: Test
testGameStateOpMonadLaws =
  let x = "foo"
      f x = (GameStateOp $ \s -> (x ++ "bar", s * 13))
      g x = (GameStateOp $ \s -> ((read x :: Int) * 12, s * 25))
      m = GameStateOp (\s -> ("15", s)) :: GameStateOp Int String
      s0 = 8
      execValue g s = fst $ runOp g s
      execState g s = snd $ runOp g s
   in TestCase $ do
        assertEqual
          "Value of returns x >>= f equals value of f x"
          True
          (execValue (return x >>= f) s0 == execValue (f x) s0)
        assertEqual
          "State of returns x >>= f equals state of f x"
          True
          (execState (return x >>= f) s0 == execState (f x) s0)
        assertEqual
          "Value of m >>= return equals value of m"
          True
          ((execValue (m >>= return) s0) == (execValue m s0))
        assertEqual
          "State of m >>= return equals state of m"
          True
          ((execState (m >>= return) s0) == (execState m s0))
        assertEqual
          "Value of (m >>= f) >>= g equals value of m >>= (\\x -> f x >>= g)"
          True
          ((execValue ((m >>= f) >>= g) s0) == (execValue (m >>= (\x -> f x >>= g)) s0))
        assertEqual
          "State of (m >>= f) >>= g equals state of m >>= (\\x -> f x >>= g)"
          True
          ((execState ((m >>= f) >>= g) s0) == (execState (m >>= (\x -> f x >>= g)) s0))

testGameStateOpHistory :: Test
testGameStateOpHistory =
  let initialState = "321"
      gsoh1 = (GameStateOpHistory $ \s0 -> (0, s0 ++ "a"))
      gsoh2 = (GameStateOpHistory $ \s0 -> (0, s0 ++ "b"))
      gsoh3 = (GameStateOpHistory $ \s0 -> (0, s0 ++ "c"))
      (v, s) =
        runOpHistory gsoh3 . snd $
          runOpHistory gsoh2 . snd $
            runOpHistory gsoh1 initialState
   in TestCase $ assertEqual "Returns correct final state" "321abc" s

testGameStateOpHistoryFunctor :: Test
testGameStateOpHistoryFunctor =
  let gso1 = (+ 5) <$> (GameStateOpHistory $ \s0 -> (0 :: Int, s0)) :: GameStateOpHistory Char Int
      gso2 = (GameStateOpHistory $ \s0 -> (5 :: Int, s0)) :: GameStateOpHistory Char Int
      s0 = "init"
      (v1, s1) = runOpHistory gso1 s0
      (v2, s2) = runOpHistory gso2 s0
   in TestCase $
        do
          assertEqual
            "Returns correct value for f <$> M"
            v2
            v1
          assertEqual
            "Returns correct state for f <$> M"
            s2
            s1

testGameStateOpHistoryFunctorLaws :: Test
testGameStateOpHistoryFunctorLaws =
  let f = (* 15)
      g = (+ 7)
      gso = (GameStateOpHistory $ \s0 -> (v0, s0))
      v0 = 1
      s0 = "init"
   in TestCase $
        do
          assertEqual
            "Returns identical value for id <$> M"
            v0
            (fst $ runOpHistory (id <$> gso) s0)
          assertEqual
            "Returns identical state for f <$> M"
            s0
            (snd $ runOpHistory (id <$> gso) s0)
          assertEqual
            "Returns same value for f.g <$> M and f <$> (g <$> M)"
            True
            ( (fst $ runOpHistory (f . g <$> gso) s0)
                == (fst $ runOpHistory (f <$> (g <$> gso)) s0)
            )
          assertEqual
            "Returns correct value for f.g <$> M"
            120
            (fst $ runOpHistory (f . g <$> gso) s0)
          assertEqual
            "Returns same state for f.g <$> M and f <$> (g <$> M)"
            True
            ( (snd $ runOpHistory (f . g <$> gso) s0)
                == (snd $ runOpHistory (f <$> (g <$> gso)) s0)
            )
          assertEqual
            "Returns correct state for f.g <$> M"
            "init"
            (snd $ runOpHistory (f . g <$> gso) s0)

testGameStateOpHistoryApplicative :: Test
testGameStateOpHistoryApplicative =
  let f = GameStateOpHistory (\s -> ((* 11), s ++ "!"))
      g = GameStateOpHistory (\s -> (13, s))
      s0 = "initial"
      r = f <*> g
      (v, s) = runOpHistory r s0
   in TestCase $
        do
          assertEqual
            "Returns correct value for f <*> g"
            143
            v
          assertEqual
            "Returns correct state for f <*> g"
            "initial!"
            s

testGameStateOpHistoryApplicativeLaws :: Test
testGameStateOpHistoryApplicativeLaws =
  let f = (* 11) :: Int -> Int
      g = GameStateOpHistory (\s -> (13, s)) :: GameStateOpHistory Char Int
      u = GameStateOpHistory (\s -> ((* 5), s)) :: GameStateOpHistory Char (Int -> Int)
      v = GameStateOpHistory (\s -> ((* 18), s)) :: GameStateOpHistory Char (Int -> Int)
      w = GameStateOpHistory (\s -> (1, s))
      h = 7 :: Int
      s0 = "initial"
      execValue g s = fst $ runOpHistory g s
      execState g s = snd $ runOpHistory g s
   in TestCase $
        do
          assertEqual
            "Value of (pure f) <*> g equals value of f <$> g"
            (execValue (pure f <*> g) s0)
            (execValue (f <$> g) s0)
          assertEqual
            "State of (pure f) <*> g equals state of f <$> g"
            (execState (pure f <*> g) s0)
            (execState (f <$> g) s0)
          assertEqual
            "Value of pure (.) <*> u <*> v <*> w equals value of u <*> (v <*> w)"
            (execValue (pure (.) <*> u <*> v <*> w) s0)
            (execValue (u <*> (v <*> w)) s0)
          assertEqual
            "State of pure (.) <*> u <*> v <*> w equals state of u <*> (v <*> w)"
            (execState (pure (.) <*> u <*> v <*> w) s0)
            (execState (u <*> (v <*> w)) s0)
          assertEqual
            "Value of pure f <*> pure x equals value of pure (f x)"
            (execValue (pure f <*> pure h) s0)
            (execValue (pure (f h)) s0)
          assertEqual
            "State of pure f <*> pure x equals state of pure (f x)"
            (execState (pure f <*> pure h) s0)
            (execState (pure (f h)) s0)
          assertEqual
            "Value of pure u <*> pure y equals value of pure ($ y) <*> u"
            (execValue (v <*> pure h) s0)
            (execValue (pure ($ h) <*> v) s0)
          assertEqual
            "State of pure u <*> pure y equals state of pure ($ y) <*> u"
            (execState (v <*> pure h) s0)
            (execState (pure ($ h) <*> v) s0)

testGameStateOpHistoryMonad :: Test
testGameStateOpHistoryMonad =
  let f = GameStateOpHistory (\s -> (2, [x * 11 | x <- s])) :: GameStateOpHistory Int Int
      g x = (GameStateOpHistory $ \s -> (x + 5, [x * 13 | x <- s]))
      h x = (GameStateOpHistory $ \s -> (x + 17, [x * 23 | x <- s]))
      s0 = [8]
   in TestCase $ do
        assertEqual
          "Returns correct value on bind"
          (2 + 5)
          (fst $ runOpHistory (f >>= g) s0)
        assertEqual
          "Returns correct state on bind"
          ([x * 11 * 13 | x <- s0])
          (snd $ runOpHistory (f >>= g) s0)
        assertEqual
          "Returns correct value on multiple binds"
          (2 + 5 + 17)
          (fst $ runOpHistory (f >>= g >>= h) s0)
        assertEqual
          "Returns correct state on multiple binds"
          ([x * 11 * 13 * 23 | x <- s0])
          (snd $ runOpHistory (f >>= g >>= h) s0)

testGameStateOpHistoryMonadLaws :: Test
testGameStateOpHistoryMonadLaws =
  let x = "foo"
      f x = (GameStateOpHistory $ \s -> (x ++ "bar", [x * 13 | x <- s]))
      g x = (GameStateOpHistory $ \s -> ((read x :: Int) * 12, [x * 25 | x <- s]))
      m = GameStateOpHistory (\s -> ("15", s)) :: GameStateOpHistory Int String
      s0 = [8]
      execValue g s = fst $ runOpHistory g s
      execState g s = snd $ runOpHistory g s
   in TestCase $ do
        assertEqual
          "Value of returns x >>= f equals value of f x"
          True
          (execValue (return x >>= f) s0 == execValue (f x) s0)
        assertEqual
          "State of returns x >>= f equals state of f x"
          True
          (execState (return x >>= f) s0 == execState (f x) s0)
        assertEqual
          "Value of m >>= return equals value of m"
          True
          ((execValue (m >>= return) s0) == (execValue m s0))
        assertEqual
          "State of m >>= return equals state of m"
          True
          ((execState (m >>= return) s0) == (execState m s0))
        assertEqual
          "Value of (m >>= f) >>= g equals value of m >>= (\\x -> f x >>= g)"
          True
          ((execValue ((m >>= f) >>= g) s0) == (execValue (m >>= (\x -> f x >>= g)) s0))
        assertEqual
          "State of (m >>= f) >>= g equals state of m >>= (\\x -> f x >>= g)"
          True
          ((execState ((m >>= f) >>= g) s0) == (execState (m >>= (\x -> f x >>= g)) s0))

tests :: Test
tests =
  TestList
    [ TestLabel "testGameStateOp" testGameStateOp,
      TestLabel "testGameStateOpFunctor" testGameStateOpFunctor,
      TestLabel "testGameStateOpFunctorLaws" testGameStateOpFunctorLaws,
      TestLabel "testGameStateOpApplicative" testGameStateOpApplicative,
      TestLabel "testGameStateOpApplicativeLaws" testGameStateOpApplicativeLaws,
      TestLabel "testGameStateOpMonad" testGameStateOpMonad,
      TestLabel "testGameStateOpHistory" testGameStateOpHistory,
      TestLabel "testGameStateOpHistoryFunctor" testGameStateOpHistoryFunctor,
      TestLabel "testGameStateOpHistoryFunctorLaws" testGameStateOpHistoryFunctorLaws,
      TestLabel "testGameStateOpHistoryApplicative" testGameStateOpHistoryApplicative,
      TestLabel "testGameStateOpHistoryApplicativeLaws" testGameStateOpHistoryApplicativeLaws,
      TestLabel "testGameStateOpHistoryMonad" testGameStateOpHistoryMonad
    ]