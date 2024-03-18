{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stream
     ( encodeFile
     , decodeFile
     ) where

import Control.Applicative              ((<$>))
import Control.Monad.Trans.State.Strict (evalState)
import Data.Foldable                    (sum)
import Data.Map.Strict                  (Map, (!))
import Lens.Family2                     (view)
import Prelude hiding                   (sum)
import System.IO                        (withFile, IOMode(..))
import qualified Data.Map.Strict        as M

import Pipes
import Pipes.Parse
import qualified Pipes.Binary as PB
import qualified Pipes.ByteString as PBS
import qualified Pipes.Prelude    as PP

import Data.Binary hiding             (encodeFile, decodeFile)
import Data.Bits                      (setBit, testBit)
import Data.ByteString                (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import Data.Word                 (Word8)

import Encode (Direction (DLeft, DRight), Encoding, ptTable)
import PrefixTree (PrefixTree (PTLeaf, PTNode), makePT)
import Huffman (buildTree, FreqTable, listQueue)
import PriorityQueue (emptyPQ)


-- Encoding
encodeFile :: FilePath -> FilePath -> IO ()
encodeFile inp out = do
    metadata <- analyzeFile inp
    let (len, tree) = case metadata of
                        Just (l, t) -> (l, t)
                        Nothing     -> error "Empty File"
    encodeFile' inp out len tree

encodeFile' :: FilePath -> FilePath -> Int -> PrefixTree Word8 -> IO ()
encodeFile' inp out len tree =
    withFile inp ReadMode  $ \hIn  ->
    withFile out WriteMode $ \hOut -> do
      BL.hPut hOut $ encode (len, tree)
      let dirsOut   = PBS.fromHandle hIn
                  >-> bsToBytes
                  >-> encodeByte (ptTable tree)
          bsOut     = view PBS.pack . dirsBytes $ dirsOut
          pipeline  = bsOut
                  >-> PBS.toHandle hOut

      runEffect pipeline

analyzeFile :: FilePath -> IO (Maybe (Int, PrefixTree Word8))
analyzeFile fp = withFile fp ReadMode $ \hIn -> do
    let byteProducer = PBS.fromHandle hIn >-> bsToBytes
    fqs <- freqs byteProducer
    let len  = sum fqs
        tree = buildTree . listQueue $ fqs
    return $ fmap (len, ) tree
  where
    freqs :: (Monad m, Ord a) => Producer a m () -> m (FreqTable (PrefixTree a))
    freqs = PP.fold f M.empty id
      where f m x = M.insertWith (+) (makePT x) 1 m

encodeByte :: (Ord a, Monad m)
           => Map a Encoding
           -> Pipe a Direction m r
encodeByte encTable = PP.mapFoldable (encTable !)

dirsBytes :: (MonadIO m, Functor m)
          => Producer Direction m r
          -> Producer Word8     m ()
dirsBytes p = do
    (result, leftovers) <- lift $ runStateT dirsBytesP p
    case result of
      Just byte -> do
        yield byte
        dirsBytes leftovers
      Nothing   -> return ()

dirsBytesP :: (Monad m, Functor m) => Parser Direction m (Maybe Word8)
dirsBytesP = do
    isEnd <- isEndOfInput
    if isEnd
      then return Nothing
      else Just <$> go 0 0
  where
    go :: Monad m => Word8 -> Int -> Parser Direction m Word8
    go b 8 = return b
    go b i = do
      dir <- draw
      case dir of
        Just DLeft  -> go     b            (i + 1)
        Just DRight -> go     (setBit b i) (i + 1)
        Nothing     -> return b

bsToBytes :: Monad m => Pipe ByteString Word8 m r
bsToBytes = PP.mapFoldable B.unpack


-- Decoding
decodeFile :: FilePath -> FilePath -> IO ()
decodeFile inp out =
    withFile inp ReadMode  $ \hIn  ->
    withFile out WriteMode $ \hOut -> do
      let metadataPipe = PBS.fromHandle hIn

      -- consume metapipe to read in the tree/metadata
      (metadata, decodingPipe) <- runStateT PB.decode metadataPipe

      case metadata of
        Left   _          ->
          error "Corrupt metadata."
        Right (len, tree) -> do
          -- do everything with the rest
          let bytesOut  = decodingPipe >-> bsToBytes
                      >-> bytesToDirs  >-> searchPT tree
                      >-> PP.take len
              bsOut     = (view PBS.pack) bytesOut
              pipeline  = bsOut
                      >-> PBS.toHandle hOut

          runEffect pipeline

searchPT :: forall a m r. Monad m
         => PrefixTree a
         -> Pipe Direction a m r
searchPT t = searchPT' t >~ cat
  where
    searchPT' :: PrefixTree a -> Consumer' Direction m a
    searchPT' (PTLeaf x)       =
        return x
    searchPT' (PTNode pt1 pt2) = do
        dir <- await
        searchPT' $ case dir of
                      DLeft  -> pt1
                      DRight -> pt2

bytesToDirs :: Monad m => Pipe Word8 Direction m r
bytesToDirs = PP.mapFoldable byteToDirList
  where
    -- Turns a byte into a list of directions
    byteToDirList :: Word8 -> [Direction]
    byteToDirList b = map f [0..7]
      where
        f i | testBit b i = DRight
            | otherwise   = DLeft
