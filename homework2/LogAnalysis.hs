{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s =
  case words s of
    "I" : stamp : message -> LogMessage Info (read stamp :: Int) (unwords message)
    "W" : stamp : message -> LogMessage Warning (read stamp :: Int) (unwords message)
    "E" : severity : stamp : message -> LogMessage (Error (read severity :: Int)) (read stamp :: Int) (unwords message)
    _ -> Unknown s

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert _ (Node _ (Unknown _) _) = Leaf
insert m@(LogMessage _ stamp _) (Node l p@(LogMessage _ parentStamp _) r) =
  if stamp < parentStamp
    then
      Node (insert m l) p r
    else Node l p (insert m r)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m : ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity >= 50
isSevereError _ = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ m) = m
extractMessage (Unknown m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map extractMessage (filter isSevereError (inOrder (build ms)))
