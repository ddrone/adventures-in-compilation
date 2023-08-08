module Graph.Dominator where

import GHC.ST (runST)

import Graph.Defs

-- In order for vertex A to dominate vertex B, vertex B has to be accessible only
-- by going through vertex A (starting traversal from some particular root node)

dominates :: Int -> Graph -> Int -> Int -> Bool
dominates root g u v = undefined
