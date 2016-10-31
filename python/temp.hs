

front :: Eq a => a -> a -> [a] -> [a]
front _ _ []     = []
front u y (x:xs) | y /= x     = x : (front u y xs)
                 | otherwise  = u:y:xs


	-- x:(front u y xs)
-- front u y (y:xs) = u:y:xs