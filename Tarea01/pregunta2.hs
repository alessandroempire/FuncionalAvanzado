import Data.Sequence as S
import Data.Foldable as DF

data Dream b a = Dream a
               | Limbo (b,a)
               | Within a (Seq (Dream b a))
               | Nightmare b
               deriving (Show)

--Instancia Functor sobre b
instance Functor (Dream b)  where 
    fmap f (Dream a)     = Dream $ f a
    fmap f (Limbo (b,a)) = Limbo(b, f a)
    fmap f (Within a xs) = Within (f a) $ fmap (fmap f) xs
    fmap f (Nightmare b) = Nightmare b

--Instancia Foldable
instance Foldable (Dream b) where 
    foldr f z (Dream a)     = f a z
    foldr f z (Limbo (b,a)) = f a z 
    foldr f z (Within a xs) = f a (DF.foldr g z xs)
        where g x acc = DF.foldr f acc x 
    foldr f z (Nightmare _) = z
    
--Test  
testDream = Within 2 ( S.fromList [ Dream 4 , Limbo (3 , 4) ,
                                    Nightmare 68 ,
                                    Within 10
                                    (S.fromList [ Dream 2 , Limbo (4 , 3)])
                                  ])
asStrings = fmap show testDream

left  = DF.foldl (-) 0 testDream
right = DF.foldr (-) 0 testDream
