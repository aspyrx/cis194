module LogAnalysis where

import Log

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                  [(a, "")] -> Just a
                  _ -> Nothing

maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f = maybe Nothing $ \x -> Just $ f x

maybeApply2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeApply2 _ Nothing _ = Nothing
maybeApply2 _ _ Nothing = Nothing
maybeApply2 f (Just a) (Just b) = Just $ f a b

parseMessageHeader :: [String] -> Maybe (MessageType, TimeStamp, [String])
parseMessageHeader (mt : x : ys)
  | mt == "I" = maybeApply (\ts -> (Info, ts, ys)) $ maybeRead x
  | mt == "W" = maybeApply (\ts -> (Warning, ts, ys)) $ maybeRead x
  | mt == "E" =
      case ys of
        (t : rest) -> maybeApply2 (\c ts -> (Error c, ts, rest)) (maybeRead x) (maybeRead t)
        _ -> Nothing
  | otherwise = Nothing

parseMessage :: String -> LogMessage
parseMessage s =
    case parseMessageHeader . words $ s of
      Just (mt, ts, rest) -> LogMessage mt ts (unwords rest)
      _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m@(LogMessage _ a _) t =
    case t of
      Leaf -> Node Leaf m Leaf
      Node l n@(LogMessage _ b _) r ->
          if a < b
             then Node (insert m l) n r
             else Node l n (insert m r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ m : inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l = [m | (LogMessage (Error c) _ m) <- inOrder $ build l, c > 50]

