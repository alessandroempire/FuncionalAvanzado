import Data.Map as M
import Data.Sequence as S
import Data.Monoid

newtype MultiMap k v = MultiMap (M.Map k (S.Seq v))
    deriving (Show)

instance Ord k => Monoid (MultiMap k v) where
    mempty = MultiMap $ M.empty
    mappend (MultiMap a) (MultiMap b) = MultiMap $ M.unionWith mappend a b 

--Ejemplo
