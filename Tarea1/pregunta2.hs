import Data.Sequence

data Dream b a = Dream a
               | Limbo (b,a)
               | Within a (Seq (Dream b a))
               | Nightmare b
               deriving (Show)

--Instancia Functor

--Instancia Foldable
