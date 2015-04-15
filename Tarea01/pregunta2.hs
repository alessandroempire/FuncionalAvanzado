import Data.Sequence

data Dream b a = Dream a
               | Limbo (b,a)
               | Within a (Seq (Dream b a))
               | Nightmare b
               deriving (Show)

dreamMap :: (b -> c) -> Dream b a -> Dream c a
dreamMap f (Dream a)     = Dream a
dreamMap f (Limbo (b,a)) = Limbo(f b, a)
dreamMap f (Within a xs) = Within a $ fmap (dreamMap f) xs
dreamMap f (Nightmare b) = Nightmare $ f b

--newtype Sueno b = Dream

--Instancia Functor sobre b
--instance Functor (Dream b a)  where 
--    fmap = dreamMap

--Instancia Foldable
--instance Foldable Dream b a where 
--foldMap 

--foldr
