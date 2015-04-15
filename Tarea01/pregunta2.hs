import Data.Sequence

data Dream b a = Dream a
               | Limbo (b,a)
               | Within a (Seq (Dream b a))
               | Nightmare b
               deriving (Show)

dreamMap :: (a -> c) -> Dream b a -> Dream b c
dreamMap f (Dream a)     = Dream $ f a
dreamMap f (Limbo (b,a)) = Limbo(b, f a)
dreamMap f (Within a xs) = Within (f a) $ fmap (dreamMap f) xs
dreamMap f (Nightmare b) = Nightmare $ b

--Instancia Functor sobre b
instance Functor (Dream b)  where 
    fmap = dreamMap

--Instancia Foldable
--instance Foldable Dream b a where 
--foldMap 

--foldr
