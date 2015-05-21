import Test.QuickCheck
import Control.Monad

prop_assoc x y z = (x + y) + z == x + (y + z)

isort :: Ord a => [a] -> [a]
isort = foldr ins []
  where ins x []     = [x]
        ins x (y:ys) = if x < y then (x:y:ys)
                                else y : ins x ys

prop_idempotent xs = isort (isort xs) == isort xs

prop_idempotent' xs = 
  classify (null xs) "trivial" $
  classify (not (null xs)) "mejor" $ isort (isort xs) == isort xs

prop_idempotent'' xs = 
  collect (length xs `div` 10) $ isort (isort xs) == isort xs

prop_minimum xs = 
  not (null xs) ==> head (isort xs) == minimum xs

prop_ordered xs = ordered $ isort xs
  where ordered []       = True
        ordered [x]      = True
        ordered (x:y:ys) = x <= y && ordered (y:ys)

prop_append xs ys =
  not (null xs) ==>
  not (null ys) ==>
    head (isort (xs++ys)) == min (minimum xs) (minimum ys)

data Tree a = Leaf a | Branch (Tree a) a (Tree a)
            deriving (Show,Eq)

-- Inocente
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = tree
    where tree = oneof [ liftM Leaf arbitrary,
                         liftM3 Branch tree arbitrary tree ]

-- Control de proporcion
{-
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = tree
    where tree = frequency [ (1,liftM Leaf arbitrary),
                             (3,liftM3 Branch tree arbitrary tree) ]
-}

-- Control de proporcion y finitud
{-
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = tree
    where tree            = sized tree'
          tree' 0         = liftM Leaf arbitrary
          tree' n | n > 0 = frequency
             [ (1,liftM Leaf arbitrary),
               (3,liftM3 Branch subtree arbitrary subtree) ]
                   where subtree = tree' (n `div` 2)
                   -}
                                    
main = do
  quickCheck (prop_idempotent :: [Int] -> Bool)
  quickCheck (prop_minimum :: [Int] -> Property)
  quickCheck (prop_ordered :: [Int] -> Bool)
  quickCheck (prop_append :: [Int] -> [Int] -> Property)
