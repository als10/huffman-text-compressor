module Huffman
     ( buildTree
     , FreqTable
     , listQueue
     ) where

import qualified Data.Map.Strict as M
import PriorityQueue (PriorityQueue, insertPQ, emptyPQ, popPQ)
import Weighted (Weighted, WeightedPT, makeW, mergeWPT, _wItem)
import PrefixTree (PrefixTree, makePT)


type FreqTable a = M.Map a Int

listFreq :: Ord a => [a] -> FreqTable a
listFreq = foldr (\x -> M.insertWith (+) x 1) M.empty

listQueue :: Ord a => FreqTable a -> PriorityQueue (Weighted a)
listQueue = M.foldrWithKey (\k v -> insertPQ (makeW v k)) emptyPQ

buildTree :: PriorityQueue (WeightedPT a) -> Maybe (PrefixTree a)
buildTree pq = let (t1, pq') = popPQ pq
                   (t2, pq'') = popPQ pq'
               in case (t1, t2) of
                    (Nothing, _) -> Nothing
                    (Just t1', Nothing) -> Just (_wItem t1')
                    (Just t1', Just t2') ->
                      let t3 = mergeWPT t1' t2'
                      in buildTree (insertPQ t3 pq'')

runBuildTree :: Ord a => [a] -> Maybe (PrefixTree a)
runBuildTree = buildTree . listQueue . listFreq . map makePT
