import Data.Sequence
import Data.Foldable
import Data.Monoid

data Dream b a = Dream a
               | Limbo (b,a)
               | Within a (Seq (Dream b a))
               | Nightmare b
               deriving (Show)

dreamMap :: (a -> c) -> Dream b a -> Dream b c
dreamMap f (Dream a)     = Dream $ f a
dreamMap f (Limbo (b,a)) = Limbo(b, f a)
dreamMap f (Within a xs) = Within (f a) $ fmap (dreamMap f) xs
dreamMap f (Nightmare b) = Nightmare b

--Instancia Functor sobre b
instance Functor (Dream b)  where 
    fmap = dreamMap

--Instancia Foldable
instance Foldable (Dream b) where 
    foldr f z (Dream a)     = f a z
    foldr f z (Limbo (b,a)) = f a z 
    foldr f z (Within a xs) = f a $ Data.Foldable.foldr (Data.Foldable.foldr f z) z xs
    foldr f z (Nightmare _) = z 
