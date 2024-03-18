module Weighted
     ( Weighted
     , WeightedPT
     , makeW
     , mergeWPT
     , _wItem
     ) where

import PrefixTree (PrefixTree, makePT, mergePT)


data Weighted a = WPair { _wWeight :: Int
                        , _wItem   :: a
                        } deriving (Show)

makeW :: Int -> a -> Weighted a
makeW = WPair

type WeightedPT a = Weighted (PrefixTree a)

makeWPT :: Int -> a -> WeightedPT a
makeWPT w = WPair w . makePT

mergeWPT :: WeightedPT a -> WeightedPT a -> WeightedPT a
mergeWPT (WPair w1 pt1) (WPair w2 pt2) = WPair (w1 + w2) (mergePT pt1 pt2)

instance Eq (Weighted a) where
    WPair w1 _ == WPair w2 _ = w1 == w2

instance Ord (Weighted a) where
    compare (WPair w1 _) (WPair w2 _) = compare w1 w2
