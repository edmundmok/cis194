{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Assumes that once the message type is correct, the rest of the format is correct.
parseHelper :: MessageType -> [String] -> LogMessage
parseHelper x ys = LogMessage x (read $ head ys) (unwords $ drop 1 ys)

parseMessage :: String -> LogMessage
parseMessage x
  | l == 0 =  Unknown x
  | otherwise =
    let 
      t = head xs
      ts = tail xs
    in case t of
      "I" -> parseHelper Info ts
      "W" -> parseHelper Warning ts
      "E" -> parseHelper (Error (read $ head ts)) (tail ts)
      _   -> Unknown x
  where xs = words x; l = length xs

parse :: String -> [LogMessage]
parse x = map parseMessage $ lines x