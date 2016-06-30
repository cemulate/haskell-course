module JoinList where

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving Eq

instance (Show m, Show a) => Show (JoinList m a) where
    show Empty = "Empty"
    show (Single m a) = "(Single " ++ show m ++ show a ++ ")"
    show (Append m l r) = "(Append " ++ show m ++ "\n" ++ indent 4 (show l) ++ indent 4 (show r) ++ ")"
        where
            indent n = unlines . map ((take n . repeat $ ' ') ++) . lines
