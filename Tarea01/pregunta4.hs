

data Tree a = Leaf a | Node a (Tree a) (Tree a) (Tree a)

data Breadcrumbs a = undefined

type Zipper a = (Tree a, Breadcrumbs a)

goLeft   ::
goRight  ::
goCenter ::
goBack   ::
tothetop ::
modify   ::
fouc     ::
defocus  ::
