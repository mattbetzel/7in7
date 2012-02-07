safeSecond :: [a] -> Maybe a

safeSecond (_:x:_) = Just x
safeSecond _ = Nothing
