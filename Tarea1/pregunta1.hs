import Data.List 

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile f xs =  foldr (($!) dropper) [] xs 
    where dropper x xs = if f x then xs
                                else (x : xs)

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f a bs =  (foldr step id bs) a
    where step x g a = g (f a x)
