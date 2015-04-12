
--el arbol
data Tree a = Leaf a | Branch (Tree a) (Tree a)

--fleaf   :: a -> b
--fbranch :: b -> b -> b

foldT :: (b -> b -> b) -> (a -> b) -> Tree a -> b
foldT fbranch fleaf tree = go tree
    where go (Leaf x)     = fleaf x
          go (Branch l r) = fbranch (go l) (go r)


--Practicando

data WTF a b = Foo a
             | Bar b
             | Baz [(a,b)]
             | Qux (WTF a b)

-- ver los colapsos:
foldWTF :: (a -> c)       ->
           (b -> c)       ->
           ([(a,b)] -> c) ->
           (c -> c)       ->
           WTF a b        -> 
           c
foldWTF foo bar baz qux wth = go wth
    where go (Foo a)       = foo a
          go (Bar b)       = bar b
          go (Baz [(a,b)]) = baz [(a,b)]
          go (Qux wtf)     = qux $ foldWTF foo bar baz qux wtf 

--insertion sort
insort :: Ord a => [a] -> [a]
insort xs = foldr ordenar [] xs
    where ordenar x []     = [x]
          ordenar x (y:ys) = if x < y then (x:y:ys)
                                      else y : ordenar x ys 
