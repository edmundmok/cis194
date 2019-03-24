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
      t  = head xs
      ts = tail xs
    in case t of
      "I" -> parseHelper Info ts
      "W" -> parseHelper Warning ts
      "E" -> parseHelper (Error (read $ head ts)) (tail ts)
      _   -> Unknown x
  where xs = words x; l = length xs

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Assumes that the given MessageTree t is sorted.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m@(LogMessage _ ts1 _) t =
  case t of
    Leaf -> Node Leaf m Leaf
    (Node _ (Unknown _) _) -> error "Should not have Unknown messages in MessageTree."
    (Node l m'@(LogMessage _ ts2 _) r) -> 
      case ts1 <= ts2 of
        True -> Node (insert m l) m' r
        False -> Node l m' (insert m r)

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

isRelevantMessage :: LogMessage -> Bool
isRelevantMessage (LogMessage (Error v) _ _) = v >= 50
isRelevantMessage _ = False

getMessageContent :: LogMessage -> String
getMessageContent (LogMessage _ _ s) = s
getMessageContent (Unknown s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessageContent . filter isRelevantMessage . (inOrder . build)
