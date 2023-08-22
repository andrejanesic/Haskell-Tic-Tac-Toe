module TestRoseTree (tests) where

import Control.Exception (ErrorCall (ErrorCall), catch)
import Data.Char (chr)
import GHC.IO.Exception (assertError)
import RoseTree (Rose (Node), children, elemsOnDepth, height, leaves, leavesCount, size, value)
import Test.HUnit

sample0 :: Rose Int
sample0 =
  Node
    0
    [ Node
        1
        [ Node 2 [],
          Node 3 [],
          Node 4 []
        ],
      Node
        5
        [ Node
            6
            [ Node
                7
                [ Node 8 []
                ]
            ]
        ],
      Node
        9
        [ Node 10 [],
          Node 11 []
        ]
    ]

sample1 :: Rose Int
sample1 = Node 10 []

sample2 :: Rose Int
sample2 =
  Node
    20
    [ Node 21 [],
      Node
        22
        [ Node 23 []
        ]
    ]

sample3 :: Rose Int
sample3 =
  Node
    30
    [ Node 31 [],
      Node
        32
        [ sample2
        ]
    ]

sample4 :: Rose Int
sample4 =
  Node
    1
    [ Node
        2
        [ Node 3 [],
          Node 4 [],
          Node 5 []
        ],
      Node
        6
        [ Node
            7
            [ Node 8 []
            ]
        ],
      Node 9 [],
      Node 10 []
    ]

testSize1 :: Test
testSize1 =
  TestCase $
    assertEqual
      "Should return 1 for 1 node"
      1
      (size (Node 0 []))

testSize5 :: Test
testSize5 =
  TestCase $
    assertEqual
      "Should return 5 for 5 nodes"
      5
      ( size
          ( Node
              0
              [ Node
                  1
                  [ Node 2 []
                  ],
                Node 3 [],
                Node 4 []
              ]
          )
      )

testSize4Mid :: Test
testSize4Mid =
  TestCase $
    assertEqual
      "Should return 4 for 4 nodes, also belonging to another tree"
      4
      (size sample2)

testSize7 :: Test
testSize7 =
  TestCase $
    assertEqual
      "Should return 5 for 7 nodes"
      7
      (size sample3)

testHeight0 :: Test
testHeight0 =
  TestCase $
    assertEqual
      "Should return 0 for tree with longest distance 0"
      0
      (height (Node "a" []))

testHeight4 :: Test
testHeight4 =
  TestCase $
    assertEqual
      "Should return 4 for tree with longest distance 4"
      4
      (height sample0)

testLeavesCount1 :: Test
testLeavesCount1 =
  TestCase $
    assertEqual
      "Should return 1 for tree with 1 leaf"
      1
      (leavesCount (Node 'a' []))

testLeavesCount6 :: Test
testLeavesCount6 =
  TestCase $
    assertEqual
      "Should return 6 for tree with 6 leaves"
      6
      (leavesCount sample0)

testLeaves1 :: Test
testLeaves1 =
  TestCase $
    assertEqual
      "Should return root value for tree with 1 leaf"
      [value sample1]
      (leaves sample1)

testLeaves6 :: Test
testLeaves6 =
  TestCase $
    assertEqual
      "Should return leaf values for tree with 6 leaves"
      [2, 3, 4, 8, 10, 11]
      (leaves sample0)

testElemsOnDepth0 :: Test
testElemsOnDepth0 =
  TestCase $
    assertEqual
      "Should return leaf value for tree with 1 leaf"
      [value sample1]
      (elemsOnDepth sample1 0)

testElemsOnDepth1 :: Test
testElemsOnDepth1 =
  TestCase $
    assertEqual
      "Should return [] for tree with 1 leaf"
      []
      (elemsOnDepth sample1 1)

testElemsOnDepth2 :: Test
testElemsOnDepth2 =
  TestCase $
    assertEqual
      "Should return element values on depth 2"
      [2, 3, 4, 6, 10, 11]
      (elemsOnDepth sample0 2)

testElemsOnDepth4 :: Test
testElemsOnDepth4 =
  TestCase $
    assertEqual
      "Should return element values on depth 4"
      [8]
      (elemsOnDepth sample0 4)

testFunctor :: Test
testFunctor =
  TestCase $
    assertEqual
      "Should return identical tree but with ord(v) instad of v"
      ( Node
          (chr 1)
          [ Node
              (chr 2)
              [ Node (chr 3) [],
                Node
                  (chr 4)
                  [ Node (chr 5) []
                  ]
              ],
            Node
              (chr 6)
              [ Node (chr 7) []
              ],
            Node (chr 8) []
          ]
      )
      ( fmap
          chr
          ( Node
              1
              [ Node
                  2
                  [ Node 3 [],
                    Node
                      4
                      [ Node 5 []
                      ]
                  ],
                Node
                  6
                  [ Node 7 []
                  ],
                Node 8 []
              ]
          )
      )

testFunctorLaws :: Test
testFunctorLaws =
  TestCase $
    do
      assertEqual
        "Returns the same rose tree"
        sample3
        (id <$> sample3)
      assertEqual
        "fmap (f . g) Rose returns the same result as fmap f (fmap g Rose)"
        ((chr . (+ 1)) <$> sample3)
        (chr <$> ((+ 1) <$> sample3))

testFoldlSum :: Test
testFoldlSum =
  TestCase $
    assertEqual
      "Returns sum of all numbers in the tree"
      (sum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
      ( foldl
          (+)
          0
          ( Node
              1
              [ Node 2 [],
                Node 3 [],
                Node 4 [],
                Node
                  5
                  [ Node 6 []
                  ],
                Node
                  7
                  [ Node
                      8
                      [ Node 9 []
                      ],
                    Node 10 []
                  ]
              ]
          )
      )

testFoldlConcat :: Test
testFoldlConcat =
  TestCase $
    assertEqual
      "Returns the correct sentence"
      "abcdefghij"
      ( foldl
          (++)
          []
          ( Node
              "a"
              [ Node "b" [],
                Node "c" [],
                Node "d" [],
                Node
                  "e"
                  [ Node "f" []
                  ],
                Node
                  "g"
                  [ Node
                      "h"
                      [ Node "i" []
                      ],
                    Node "j" []
                  ]
              ]
          )
      )

testFoldlMath :: Test
testFoldlMath =
  TestCase $
    assertEqual
      "Returns correct evaluation result"
      5137
      ( foldl
          (\acc v -> v acc)
          5
          ( Node
              (+ 1)
              [ Node (+ 2) [],
                Node (+ 2) [],
                Node (* 2) [],
                Node
                  (+ 5)
                  [ Node (+ 25) []
                  ],
                Node
                  (* 10)
                  [ Node
                      (* 2)
                      [ Node (* 5) []
                      ],
                    Node (+ 137) []
                  ]
              ]
          )
      )

testFoldrSum :: Test
testFoldrSum =
  TestCase $
    assertEqual
      "Returns sum of all numbers in the tree"
      (sum [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
      ( foldr
          (+)
          0
          ( Node
              1
              [ Node 2 [],
                Node 3 [],
                Node 4 [],
                Node
                  5
                  [ Node 6 []
                  ],
                Node
                  7
                  [ Node
                      8
                      [ Node 9 []
                      ],
                    Node 10 []
                  ]
              ]
          )
      )

testFoldrConcat :: Test
testFoldrConcat =
  TestCase $
    assertEqual
      "Returns the correct sentence"
      (reverse "agjhiefdcb")
      ( foldr
          (++)
          []
          ( Node
              "a"
              [ Node "b" [],
                Node "c" [],
                Node "d" [],
                Node
                  "e"
                  [ Node "f" []
                  ],
                Node
                  "g"
                  [ Node
                      "h"
                      [ Node "i" []
                      ],
                    Node "j" []
                  ]
              ]
          )
      )

testFoldrMath :: Test
testFoldrMath =
  TestCase $
    assertEqual
      "Returns correct evaluation result"
      4004
      ( foldr
          (\v acc -> v acc)
          5
          ( Node
              (+ 1)
              [ Node (+ 2) [],
                Node (+ 2) [],
                Node (* 2) [],
                Node
                  (+ 5)
                  [ Node (+ 25) []
                  ],
                Node
                  (* 10)
                  [ Node
                      (* 2)
                      [ Node (* 5) []
                      ],
                    Node (+ 137) []
                  ]
              ]
          )
      )

tests :: Test
tests =
  TestList
    [ TestLabel "testSize1" testSize1,
      TestLabel "testSize5" testSize5,
      TestLabel "testSize4Mid" testSize4Mid,
      TestLabel "testSize7" testSize7,
      TestLabel "testHeight0" testHeight0,
      TestLabel "testHeight4" testHeight4,
      TestLabel "testLeavesCount1" testLeavesCount1,
      TestLabel "testLeavesCount6" testLeavesCount6,
      TestLabel "testLeaves1" testLeaves1,
      TestLabel "testElemsOnDepth0" testElemsOnDepth0,
      TestLabel "testElemsOnDepth1" testElemsOnDepth1,
      TestLabel "testElemsOnDepth2" testElemsOnDepth2,
      TestLabel "testElemsOnDepth4" testElemsOnDepth4,
      TestLabel "testFunctor" testFunctor,
      TestLabel "testFunctorLaws" testFunctorLaws,
      TestLabel "testFoldlSum" testFoldlSum,
      TestLabel "testFoldlConcat" testFoldlConcat,
      TestLabel "testFoldlMath" testFoldlMath,
      TestLabel "testFoldrSum" testFoldrSum,
      TestLabel "testFoldrConcat" testFoldrConcat,
      TestLabel "testFoldrMath" testFoldrMath
    ]
