{-# LANGUAGE DeriveFunctor , DeriveFoldable #-}

import Data.Maybe
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

guess :: Hypothesis Double
guess = Hypothesis { c = [0.0, 0.0, 0.0] }

training :: [Sample Double]

veryClose :: Double -> Double -> Bool
veryClose v0 v1 = abs (v0 - v1) <= epsilon

addOnes :: [Sample Double] -> [Sample Double]
addOnes = map g
    where g k = k { x = 1.0:(x k)}

theta :: Hypothesis Double -> Sample Double -> Double
theta h s = foldl' (+) 0 $ zipWith (*) (c h) (x s)

cost :: Hypothesis Double -> [Sample Double] -> Double
cost h ss = f $ foldl' g (0, 0.0) ss
    where g (m, sum) z = (m+1, sum + ((theta h z) - (y z))^2 )
          f (a, b)     = b / (2*a)

test1 = Hypothesis {c=[1.0, 2.0, 3.0]}
test2 = [ Sample {x=[4.0, 5.0, 6.0], y = 1},
          Sample {x=[1.0, 2.0, 3.0], y = 2},
          Sample {x=[2.0, 3.0, 4.0], y = 3}
        ]

descend :: Double -> Hypothesis Double -> [Sample Double]
                  -> Hypothesis Double
descend alpha h ss = undefined

gd :: Double -> Hypothesis Double -> [Sample Double]
             -> [(Integer,Hypothesis Double,Double)]
gd alpha h ss = undefined

--Pregunta2----------------------------------------------------------
newtype Max a = Max {getMax :: Maybe a}
    deriving (Eq, Ord, Show)

instance (Eq a, Ord a, Monoid a) => Monoid (Max a) where
    mempty                                = Max Nothing
    mappend (Max (Just a)) (Max Nothing)  = Max $ Just a 
    mappend (Max Nothing) (Max (Just a))  = Max $ Just a
    mappend (Max (Just a)) (Max (Just b)) = Max $ Just $ max a b

--Test
--test21 = DF.foldMap (Max . Just) []
--test22 = DF.foldMap (Max . Just) ["foo", "bar", "baz"]
--test23 = DF.foldMap (Max . Just) (Node 6 [Node 42 [], Node 7 [] ])
--test24 = DF.foldMap (Max . Just) (Node [] [])

-------------------Pregunta 3----------------------------------------
data Filesystem a = File a | Directory a [Filesystem a]
    deriving (Eq, Show)

testFilesystem :: Filesystem Int
testFilesystem = 
    Directory 1 [
                 Directory 11 [
                                Directory 12 [File 14],
                                Directory 13 [File 15]
                              ],
                 File 2,
                 Directory 3 [ File 31,
                               Directory 34 [],
                               File 35
                             ],
                 Directory 5 [ File 6 ]
                ]

data Crumb a = Parent a [Filesystem a] [Filesystem a]
    deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

type Zipper a = (Filesystem a, Breadcrumbs a)

goDown   :: Zipper a -> Maybe (Zipper a)
goDown (Directory z (Directory v xs:ys), [] ) = Just $ (Directory v xs, 
                                                           (Parent v [] []):
                                                           [Parent z [] ys])
goDown (Directory z (Directory v ys:qs), 
       ((Parent k b bs) : xs))              = Just $ (Directory v ys,
                                                      (Parent v [] []):
                                                      ((Parent k b qs):xs))
goDown _                                    = Nothing

goRight  :: Zipper a -> Maybe (Zipper a)
goRight (Directory z (y:ys), []) = Just $ (Directory z ys, 
                                          [Parent z [y] []])
goRight (Directory z (y:ys), 
        ((Parent v b bs): xs))   = Just $ (Directory z ys, 
                                            ((Parent z (y:b) bs) : xs))
goRight _                        = Nothing

goLeft   :: Zipper a -> Maybe (Zipper a)
goLeft (Directory z ys, 
       ((Parent v (x:xs) bs) : cs)) = Just $ (Directory z (x:ys),
                                            ((Parent v xs bs) : cs))
goLeft _                            = Nothing

goBack   :: Zipper a -> Maybe (Zipper a)
goBack (Directory z ys, (Parent _ [] []):(Parent b xs cs):ls) = 
            Just $ (Directory b ((Directory z ys):cs), (Parent b xs []):ls)
goBack (Directory z ys, (Parent q (x:xs) []):(Parent b ks cs):ls ) = 
            goBack $ (Directory z (x:ys), (Parent q xs []):(Parent b ks cs):ls)
goBack _ = Nothing

tothetop :: Zipper a -> Maybe (Zipper a)
tothetop (f , xs@[Parent q [] []]) = Just $ (f, xs)
tothetop (Directory z ys , [Parent q (x:xs) []]) = 
             tothetop $ (Directory z (x:ys), [Parent q xs []])
tothetop f = tothetop $ fromJust $ goBack f

modify   :: (a -> a) -> Zipper a -> Maybe (Zipper a)
modify f (File a, bs)         = Just (File (f a), bs)
modify f (Directory a xs, bs) = Just (Directory (f a) xs, bs)

focus    :: Filesystem a -> Zipper a
focus f = (f, [])

defocus  :: Zipper a -> Filesystem a
defocus (f, _) = f

---------------------------------------------------------------------
training = [
  Sample { x = [  0.1300098690745405, -0.2236751871685913 ], y = 399900 },
  Sample { x = [ -0.5041898382231769, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.502476363836692, -0.2236751871685913 ], y = 369000 },
  Sample { x = [ -0.7357230646969468, -1.537766911784067 ], y = 232000 },
  Sample { x = [  1.257476015381594, 1.090416537446884 ], y = 539900 },
  Sample { x = [ -0.01973172848186497, 1.090416537446884 ], y = 299900 },
  Sample { x = [ -0.5872397998931161, -0.2236751871685913 ], y = 314900 },
  Sample { x = [ -0.7218814044186236, -0.2236751871685913 ], y = 198999 },
  Sample { x = [ -0.7810230437896409, -0.2236751871685913 ], y = 212000 },
  Sample { x = [ -0.6375731099961096, -0.2236751871685913 ], y = 242500 },
  Sample { x = [ -0.07635670234773261, 1.090416537446884 ], y = 239999 },
  Sample { x = [ -0.0008567371932424295, -0.2236751871685913 ], y = 347000 },
  Sample { x = [ -0.1392733399764744, -0.2236751871685913 ], y = 329999 },
  Sample { x = [  3.117291823687202,   2.40450826206236 ], y = 699900 },
  Sample { x = [ -0.9219563120780225, -0.2236751871685913 ], y = 259900 },
  Sample { x = [  0.3766430885792084,  1.090416537446884 ], y = 449900 },
  Sample { x = [ -0.856523008944131,  -1.537766911784067 ], y = 299900 },
  Sample { x = [ -0.9622229601604173, -0.2236751871685913 ], y = 199900 },
  Sample { x = [  0.7654679091248329,  1.090416537446884 ], y = 499998 },
  Sample { x = [  1.296484330711414,   1.090416537446884 ], y = 599000 },
  Sample { x = [ -0.2940482685431793, -0.2236751871685913 ], y = 252900 },
  Sample { x = [ -0.1417900054816241, -1.537766911784067 ], y = 255000 },
  Sample { x = [ -0.4991565072128776, -0.2236751871685913 ], y = 242900 },
  Sample { x = [ -0.04867338179108621, 1.090416537446884 ], y = 259900 },
  Sample { x = [  2.377392165173198,  -0.2236751871685913 ], y = 573900 },
  Sample { x = [ -1.133356214510595,  -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.6828730890888036, -0.2236751871685913 ], y = 464500 },
  Sample { x = [  0.6610262906611214, -0.2236751871685913 ], y = 469000 },
  Sample { x = [  0.2508098133217248, -0.2236751871685913 ], y = 475000 },
  Sample { x = [  0.8007012261969283, -0.2236751871685913 ], y = 299900 },
  Sample { x = [ -0.2034483103577911, -1.537766911784067 ], y = 349900 },
  Sample { x = [ -1.259189489768079,  -2.851858636399542 ], y = 169900 },
  Sample { x = [  0.04947657290975102, 1.090416537446884 ], y = 314900 },
  Sample { x = [  1.429867602484346,  -0.2236751871685913 ], y = 579900 },
  Sample { x = [ -0.2386816274298865,  1.090416537446884 ], y = 285900 },
  Sample { x = [ -0.7092980768928753, -0.2236751871685913 ], y = 249900 },
  Sample { x = [ -0.9584479619026928, -0.2236751871685913 ], y = 229900 },
  Sample { x = [  0.1652431861466359,  1.090416537446884 ], y = 345000 },
  Sample { x = [  2.78635030976002,    1.090416537446884 ], y = 549000 },
  Sample { x = [  0.202993168723881,   1.090416537446884 ], y = 287000 },
  Sample { x = [ -0.4236565420583874, -1.537766911784067 ], y = 368500 },
  Sample { x = [  0.2986264579195686, -0.2236751871685913 ], y = 329900 },
  Sample { x = [  0.7126179335166897,  1.090416537446884 ], y = 314000 },
  Sample { x = [ -1.007522939253111,  -0.2236751871685913 ], y = 299000 },
  Sample { x = [ -1.445422737149154,  -1.537766911784067 ], y = 179900 },
  Sample { x = [ -0.1870899845743182,  1.090416537446884 ], y = 299900 },
  Sample { x = [ -1.003747940995387,  -0.2236751871685913 ], y = 239500 } ]
