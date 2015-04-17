
data Tree a = Leaf | Branch a (Tree a) (Tree a)
    deriving (Eq, Show)

testTree :: Tree Char
testTree =
    Branch 'A'
         ( Branch 'R'
              (Branch 'P' Leaf Leaf)
              (Branch 'E' Leaf Leaf)
         )
         ( Branch 'E'
            (Branch 'R' Leaf Leaf)
            (Branch 'A' Leaf Leaf)
         )

data Crumb a = LeftCrumb  a (Tree a)
             | RightCrumb a (Tree a)
    deriving (Eq, Show)

type BreadCrumbs a = [Crumb a]

type Zipper a = (Tree a, BreadCrumbs a)

goLeft :: Zipper a -> Zipper a
goLeft ((Branch x l r), bs) = (l, (LeftCrumb x r):bs)

goRight :: Zipper a -> Zipper a
goRight ((Branch x l r), bs) = (r, (RightCrumb x l):bs)

goBack :: Zipper a -> Zipper a 
goBack (t, (RightCrumb x l):bs) = (Branch x l t, bs)
goBack (t, (LeftCrumb  x r):bs) = (Branch x t r, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Branch x l r, bs) = (Branch (f x) l r, bs)
modify f (Leaf, bs)         = (Leaf, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

tothetop :: Zipper a -> Zipper a
tothetop (t, []) = (t, [])
tothetop z       = tothetop $ goBack z

focus :: Tree a -> Zipper a
focus t = (t, [])

defocus :: Zipper a -> Tree a
defocus (t, _) = t
