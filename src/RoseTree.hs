{-# LANGUAGE InstanceSigs #-}
module RoseTree (Rose (Node), size, height, leavesCount, leaves, elemsOnDepth, nodesOnDepth, value, children) where

data Rose a = Node {value :: a, children :: [Rose a]}

size :: Rose a -> Int
size (Node v []) = 1
size (Node v xs) = 1 + foldl (\acc x -> acc + size x) 0 xs

height :: Rose a -> Int
height (Node v []) = 0
height (Node v xs) = 1 + foldl (\acc x -> max acc (height x)) 0 xs

leavesCount :: Rose a -> Int
leavesCount (Node v []) = 1
leavesCount (Node v xs) = sum $ map leavesCount xs

leaves :: Rose a -> [a]
leaves (Node v []) = [v]
leaves (Node v xs) = concatMap leaves xs

elemsOnDepth :: (Integral b) => Rose a -> b -> [a]
elemsOnDepth (Node v xs) d
  | d < 0 = error "Depth must be a non-negative integral"
  | d == 0 = [v]
  | otherwise =
      if null xs
        then []
        else concatMap (`elemsOnDepth` (d - 1)) xs

nodesOnDepth :: (Integral b) => Rose a -> b -> [Rose a]
nodesOnDepth n@(Node v xs) d
  | d < 0 = error "Depth must be a non-negative integral"
  | d == 0 = [n]
  | otherwise =
      if null xs
        then []
        else concatMap (`nodesOnDepth` (d - 1)) xs

instance Functor Rose where
  fmap f (Node v xs) = Node (f v) $ fmap f <$> xs

instance (Eq a) => Eq (Rose a) where
  (==) :: Eq a => Rose a -> Rose a -> Bool
  (Node a xs) == (Node b ys) = a == b && xs == ys

instance (Show a) => Show (Rose a) where
  show n@(Node v xs) = helper n 0
    where
      helper :: (Show a) => Rose a -> Int -> String
      helper (Node v xs) s =
        replicate s ' '
          ++ "{\n"
          ++ replicate (s + 2) ' '
          ++ "value: "
          ++ show v
          ++ "\n"
          ++ replicate (s + 2) ' '
          ++ "children: "
          ++ ( if null xs
                 then "[]\n"
                 else
                   "[\n"
                     ++ concatMap (`helper` (s + 4)) xs
                     ++ replicate (s + 2) ' '
                     ++ "]\n"
             )
          ++ replicate s ' '
          ++ "}\n"

instance Foldable Rose where
  foldl f acc (Node v xs) = foldl (foldl f) (f acc v) xs
  foldr f acc (Node v xs) = foldr (flip (foldr f)) (f v acc) xs




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