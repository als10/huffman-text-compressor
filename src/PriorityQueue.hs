module PriorityQueue
     ( PriorityQueue
     , insertPQ
     , emptyPQ
     , popPQ
     ) where


data SkewHeap a = SEmpty
                | SNode a (SkewHeap a) (SkewHeap a)
                deriving (Show, Eq)

makeSH :: a -> SkewHeap a
makeSH x = SNode x SEmpty SEmpty

popSH :: Ord a => SkewHeap a -> (Maybe a, SkewHeap a)
popSH SEmpty          = (Nothing, SEmpty)
popSH (SNode r h1 h2) = (Just r, mergeSH h1 h2)

mergeSH :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
mergeSH SEmpty h = h
mergeSH h SEmpty = h
mergeSH hA@(SNode xA lA rA) hB@(SNode xB lB rB)
    | xA < xB    = SNode xA (mergeSH rA hB) lA
    | otherwise  = SNode xB (mergeSH rB hA) lB

sizeSH :: SkewHeap a -> Int
sizeSH SEmpty = 0
sizeSH (SNode _ l r) = 1 + sizeSH l + sizeSH r


newtype PriorityQueue a = PQ (SkewHeap a) deriving Show

emptyPQ :: PriorityQueue a
emptyPQ = PQ SEmpty

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ h) = PQ (mergeSH h (makeSH x))

popPQ :: Ord a => PriorityQueue a -> (Maybe a, PriorityQueue a)
popPQ (PQ h) = (res, PQ h')
  where (res, h') = popSH h

sizePQ :: PriorityQueue a -> Int
sizePQ (PQ h) = sizeSH h
