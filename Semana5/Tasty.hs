module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU

data Tree a = Leaf | Node a (Tree a) (Tree a)
            deriving (Show,Eq)

tinsert x Leaf           = Node x Leaf Leaf
tinsert x t@(Node z l r) =
   if x == z then t
             else if x < z then Node z (tinsert x l) r
                           else Node z l (tinsert x r)

testInsertOnLeaf :: TestTree
testInsertOnLeaf = HU.testCase "tinsert on Leaf" $
  assertEqual "wrong!" (tinsert 42 Leaf)
                       (Node 42 Leaf Leaf)

allTests :: TestTree
allTests = testGroup "Test all the tests!" [
   testGroup "Test tinsert" [ testInsertOnLeaf ] ]

main :: IO ()
main = defaultMain allTests
