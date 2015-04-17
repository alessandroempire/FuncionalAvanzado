import Data.Maybe

data Tree a = Leaf a | Node a (Tree a) (Tree a) (Tree a)
    deriving (Eq, Show)

data Crumb a = LeftCrumb   a (Tree a) (Tree a)
             | CenterCrumb a (Tree a) (Tree a)
             | RightCrumb  a (Tree a) (Tree a)
    deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node a l c r, bs) = Just (l, (LeftCrumb a c r):bs)
goLeft (Leaf _ , _)       = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node a l c r, bs) = Just (r, (RightCrumb a l c):bs)
goRight (Leaf _ , _)       = Nothing

goCenter :: Zipper a -> Maybe (Zipper a)
goCenter (Node a l c r, bs) = Just (c, (CenterCrumb a l r):bs)
goCenter (Leaf _ , _)       = Nothing

goBack :: Zipper a -> Maybe (Zipper a)
goBack (t, (LeftCrumb   a c r):bs) = Just (Node a t c r,bs)
goBack (t, (RightCrumb  a l c):bs) = Just (Node a l c t,bs)
goBack (t, (CenterCrumb a l r):bs) = Just (Node a l t r,bs)
goBack (t , [])                    = Nothing

tothetop :: Zipper a -> Maybe (Zipper a)
tothetop (t, []) = Just (t, [])
tothetop z = tothetop $ fromJust $ goBack z

modify :: (a -> a) -> Zipper a -> Maybe (Zipper a)
modify f (Node a l c r, bs) = Just (Node (f a) l c r, bs)
modify f (Leaf v, bs)       = Just (Leaf (f v), bs)

focus :: Tree a -> Maybe (Zipper a)
focus t = Just (t, [])

defocus :: Zipper a -> Maybe (Tree a)
defocus (t, _) = Just t
