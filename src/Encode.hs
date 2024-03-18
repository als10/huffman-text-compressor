{-# LANGUAGE DeriveGeneric #-}
module Encode
     ( encodeAll
     , decodeAll
     , Encoding
     , Direction (DLeft, DRight)
     , ptTable
     ) where

import qualified Data.Map.Strict as M
import Data.List (unfoldr)
import GHC.Generics (Generic)
import PrefixTree (PrefixTree (PTLeaf, PTNode))

data Direction = DLeft
               | DRight
               deriving (Show, Eq, Generic)

type Encoding = [Direction]

ptTable :: Ord a => PrefixTree a -> M.Map a Encoding
ptTable pt = go pt []
  where
    go (PTLeaf x) enc       = x `M.singleton` reverse enc
    go (PTNode pt1 pt2) enc = go pt1 (DLeft  : enc) <>
                              go pt2 (DRight : enc)

lookupPTTable :: Ord a => M.Map a Encoding -> a -> Maybe Encoding
lookupPTTable = flip M.lookup

encodeAll :: Ord a => PrefixTree a -> [a] -> Maybe Encoding
encodeAll pt xs = concat <$> mapM (lookupPTTable tb) xs
  where tb = ptTable pt

decodePT :: PrefixTree a -> Encoding -> Maybe (a, Encoding)
decodePT (PTLeaf x)       ds     = Just (x, ds)
decodePT (PTNode pt1 pt2) (d:ds) = case d of
                                     DLeft  -> decodePT pt1 ds
                                     DRight -> decodePT pt2 ds
decodePT (PTNode _ _)     []     = Nothing

decodeAll' :: PrefixTree a -> Encoding -> [a]
decodeAll' pt = unfoldr (decodePT pt)

decodeAll :: PrefixTree a -> Encoding -> Maybe [a]
decodeAll (PTLeaf _) _  = Nothing
decodeAll pt         ds = Just (decodeAll' pt ds)
