module PrefixTree 
     ( PrefixTree (PTLeaf, PTNode)
     , makePT
     , mergePT
     ) where

import Data.Binary (Binary, Get, get, Put, put)


data PrefixTree a = PTLeaf a
                  | PTNode (PrefixTree a) (PrefixTree a)
                  deriving (Show, Eq, Ord)

putPT :: Binary a => PrefixTree a -> Put
putPT (PTLeaf x) = do
    put True
    put x
putPT (PTNode pt1 pt2) = do
    put False
    put pt1
    put pt2

getPT :: Binary a => Get (PrefixTree a)
getPT = do
    isLeaf <- get
    if isLeaf
    then PTLeaf <$> get
    else PTNode <$> get <*> get

instance Binary a => Binary (PrefixTree a) where
    put = putPT
    get = getPT

makePT :: a -> PrefixTree a
makePT = PTLeaf

mergePT :: PrefixTree a -> PrefixTree a -> PrefixTree a
mergePT = PTNode
