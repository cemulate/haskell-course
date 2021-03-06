module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage s = case parts of
    ("E":code:time:rest)    -> LogMessage (Error (read code)) (read time) (unwords rest)
    ("I":time:rest)         -> LogMessage Info (read time) (unwords rest)
    ("W":time:rest)         -> LogMessage Warning (read time) (unwords rest)
    other                   -> Unknown (unwords other)
    where
        parts = (words s)

parse :: String -> [LogMessage]
parse input = map parseMessage (lines input)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree    = tree
insert newMessage Leaf     = Node Leaf newMessage Leaf
insert newMessage tree
    | newTime <= time      = Node (insert newMessage left) message right
    | otherwise            = Node left message (insert newMessage right)
    where
        (LogMessage _ newTime _) = newMessage
        (Node left message@(LogMessage _ time _) right) = tree

build :: [LogMessage] -> MessageTree
build []       = Leaf
build (x:xs)   = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                         = []
inOrder (Node left message right)    = (inOrder left) ++ [message] ++ (inOrder right)

extractImportant :: [LogMessage] -> [LogMessage]
extractImportant []       = []
extractImportant (x:xs)   = (messageIfImportant x) ++ (extractImportant xs)
    where
        messageIfImportant x = case x of
            message@(LogMessage (Error sev) _ _) -> (if (sev >= 50) then [message] else [])
            other                                -> []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = map toMessage (extractImportant (inOrder (build list)))
    where
        toMessage (LogMessage _ _ message) = message
