import Data.List

encode :: Eq a => [a] -> [(Int,a)]
encode ls = map (\x -> (length x, head x)) $ group ls

decode :: [(Int,a)] -> [a]
decode ls = concat $ map (\(x,y) -> replicate x y) ls
