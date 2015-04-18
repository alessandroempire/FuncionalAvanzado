{-# LANGUAGE DeriveFunctor , DeriveFoldable #-}

import Data.List
import Data.Functor
import Data.Monoid
import qualified Data.Foldable as DF (foldMap)
import Data.Tree
--import Graphics.Rendering.Chart.Easy
--import Graphics.Rendering.Chart.Backend.Cairo

--Primera parte

data Sample a = Sample { x :: [a], y :: a }
    deriving (Show)

data Hypothesis a = Hypothesis { c :: [a] }
    deriving (Show)

alpha :: Double
alpha = 0.03

epsilon :: Double
epsilon = 0.0000001

guess : : Hypothesis Double
guess = Hypothesis { c = [0.0, 0.0, 0.0] }

training :: [Sample Double]

--Pregunta2
newtype Max a = Max {getMax :: a}
    deriving (Eq, Read, Show)

instance (Eq a) => Monoid (Max a) where
    mempty                  = undefined
    mappend (Max a) (Max b) = undefined

--Test
--test21 = foldMap (Max . Just) []
--test22 = foldMap (Max . Just) ["foo", "bar", "baz"]
--test23 = foldMap (Max . Just) (Node 6 [Node 42 [], Node 7 [] ])
--test24 = foldMap (Max . Just) (Node [] [])

-- Pregunta 3
data Filesystem a = File a | Directory a [Filesystem a]
    deriving (Eq, Show)

data Crumb a = Parent a [Filesystem a] [Filesystem a]

type Breadcrumbs a = [Crumb a]

type Zipper a = (Filesystem a, Breadcrumbs a)
