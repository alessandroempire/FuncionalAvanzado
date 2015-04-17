import Data.Sequence
import Data.Foldable as DF
import Data.Monoid

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
