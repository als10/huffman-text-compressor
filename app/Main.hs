module Main (main) where

import Stream (encodeFile, decodeFile)
import System.Environment (getArgs)

usage :: String
usage = "Usage: huffman-compression <encode|decode> <input> <output>"

main :: IO ()
main = do
    args <- getArgs
    case args of
        action:inp:out:_ -> case action of
            "encode" -> encodeFile inp out
            "decode" -> decodeFile inp out
            _        -> error usage
        _ -> error usage
