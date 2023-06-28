{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

-- | In this example, each path within a tree is indexed by the list of nodes
-- above it, all the way to the root. That is, given the tree:
--
--     3
--       2
--         4
--       5
--
-- the node 3 is at index [] (at the root)
-- the node 3 is at index [3]
-- the node 5 is at index [3]
-- the node 4 is at index [2,3]
module Main where

import Control.Lens               (indexed)
import Control.Lens.IndexedPlated
import Data.Foldable              (for_, traverse_)
import Data.Tree

-- | This instance creates the traversal given a root note and its corresponding
-- index. Note that we're not recursively traversing the entire tree, but only
-- visiting the direct descendents.
instance IndexedPlated [a] (Tree a) where
  indexedPlate p f (Node label children) =
    Node label <$> traverse (indexed f (label:p)) children

-- | Prints a tree.
display :: Show a => Tree a -> IO ()
display = go 0
  where
    go indent (Node label subs) = do
      putStrLn $ replicate indent ' ' <> show label
      traverse_ (go $ indent + 2) subs


-- | An example tree of ints.
baseTree :: Tree Int
baseTree = go 0 1
  where
    go :: Int -> Int -> Tree Int
    go depth n
      | depth < 3 = Node n [go (depth + 1) (n * 2), go (depth + 1) (n * 3)]
      | otherwise = Node n []

main :: IO ()
main = do
  -- 1
  -- 2
  --   4
  --     8
  --     12
  --   6
  --     12
  --     18
  -- 3
  --   6
  --     12
  --     18
  --   9
  --     18
  --     27
  putStrLn "# base tree"
  display baseTree

  -- [1]
  -- 2
  --   4
  --     8
  --     12
  --   6
  --     12
  --     18
  -- [1]
  -- 3
  --   6
  --     12
  --     18
  --   9
  --     18
  --     27
  putStrLn ""
  putStrLn "# children"
  for_ (ichildren @[Int] [] baseTree) \(p, t) -> do
    print p
    display t

  -- 0
  --   1
  --     2
  --       3
  --       3
  --     2
  --       3
  --       3
  --   1
  --     2
  --       3
  --       3
  --     2
  --       3
  --       3
  putStrLn ""
  putStrLn "# rewrite"
  let toDepth p (Node l cs)
        | length p == l = Nothing
        | otherwise     = Just $ Node (length p) cs
  display $ irewrite @[Int] toDepth [] baseTree

  -- [1]
  -- 2
  --   4
  --     8
  --     12
  --   6
  --     12
  --     18
  -- [2,1]
  -- 4
  --   8
  --   12
  -- [4,2,1]
  -- 8
  -- [4,2,1]
  -- 12
  -- [2,1]
  -- 6
  --   12
  --   18
  -- [6,2,1]
  -- 12
  -- [6,2,1]
  -- 18
  -- [1]
  -- 3
  --   6
  --     12
  --     18
  --   9
  --     18
  --     27
  -- [3,1]
  -- 6
  --   12
  --   18
  -- [6,3,1]
  -- 12
  -- [6,3,1]
  -- 18
  -- [3,1]
  -- 9
  --   18
  --   27
  -- [9,3,1]
  -- 18
  -- [9,3,1]
  -- 27
  putStrLn ""
  putStrLn "# universe (- root)"
  for_ (tail $ iuniverse @[Int] [] baseTree) \(p, t) -> do
    print p
    display t
