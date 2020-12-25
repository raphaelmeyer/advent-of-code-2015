module Day01.Floor
  ( whatFloor
  , enterBasement
  )
where

whatFloor :: String -> Int
whatFloor description = sum $ map upDown description

enterBasement :: String -> Maybe Int
enterBasement description = walk 0 1 $ map upDown description

walk :: Int -> Int -> [Int] -> Maybe Int
walk _ _ [] = Nothing
walk f n (x : xs) | f + x < 0 = Just n
                  | otherwise = walk (f + x) (n + 1) xs

upDown :: Char -> Int
upDown x = case x of
  '(' -> 1
  ')' -> -1
  _   -> error "invalid value"
