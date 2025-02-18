module Task3Suite where

import Test.Tasty hiding (TestTree)
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck

import Prelude hiding (compare, Ordering(..))

import Task1
import Task3

import Task1Suite

import Data.List (sortBy, nubBy, deleteBy)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (isJust)


task3Tests :: T.TestTree
task3Tests = testGroup "Task3"
  [ testProperty "mapToList tree == sort (mapToList tree)" $
      withMaxSuccess 100 $ counterexample "unexpected result for" $
        \(TestMap tree) ->
          mapToList (tree :: Map Int Char) === sortBy (comparing fst) (mapToList tree)

  , testProperty "mapToList (listToMap list) == sort list" $
      withMaxSuccess 100 $ counterexample "unexpected result for" $
        \xs ->
          let uxs = nubBy ((==) `on` fst) xs
          in mapToList (listToMap (uxs :: [(Int, Char)])) === sortBy (comparing fst) uxs

  , testProperty "mapToList (listToMap list) == sort list (without `Ord v`)" $
      withMaxSuccess 100 $ counterexample "unexpected result for" $
        \xs ->
          let uxs = nubBy ((==) `on` fst) xs
          in mapToList (listToMap (uxs :: [(Int, Custom)])) === sortBy (comparing fst) uxs

  , testProperty "mlookup" $
      withMaxSuccess 1000 $ counterexample "unexpected mlookup result of" $
        \(x, TestMap tree) ->
          classify (isJust (x `lookup` mapToList tree)) "contains" $
            mlookup x (tree :: Map Int Char) === x `lookup` mapToList tree

  , testProperty "mapToList (minsert k v tree) == sort uniqueKey ((k, v) : mapToList tree)" $
      withMaxSuccess 1000 $ counterexample "unexpected result for" $
        \((k, v), TestMap tree) ->
          classify (isJust (k `lookup` mapToList tree)) "contains" $
            mapToList (minsert k v (tree :: Map Int Char)) ===
              case k `lookup` mapToList tree of
                Nothing -> sortBy (comparing fst) ((k, v) : mapToList tree)
                Just _  -> sortBy (comparing fst) ((k, v) : deleteBy ((==) `on` fst) (k, undefined) (mapToList tree))

  , testProperty "mapToList (mdelete k tree) == deleteKey k (mapToList tree)" $
      withMaxSuccess 1000 $ counterexample "unexpected result for" $
        \(k, TestMap tree) ->
          classify (isJust (k `lookup` mapToList tree)) "contains" $
            mapToList (mdelete k (tree :: Map Int Char)) === deleteBy ((==) `on` fst) (k, undefined) (mapToList tree)
  ]

newtype TestMap k v = TestMap { unTestMap :: Map k v }

instance (Show k, Show v) => Show (TestMap k v) where
  show (TestMap x) = show x

instance (Bounded k, Enum k, Ord k, Arbitrary k, Arbitrary v) => Arbitrary (TestMap k v) where
  arbitrary = sized (arbitrarySizedMap Nothing)
  shrink = map (TestMap . unTestTree) . shrink . TestTree . unTestMap

arbitrarySizedMap :: (Bounded k, Enum k, Ord k, Arbitrary k, Arbitrary v) => Maybe (k, k) -> Int -> Gen (TestMap k v)
arbitrarySizedMap _ 0 = pure $ TestMap Leaf
arbitrarySizedMap (Just (l, r)) _ | l > r = pure $ TestMap Leaf
arbitrarySizedMap bounds m = do
  k <- maybe arbitrary chooseEnum bounds
  v <- arbitrary
  let lbounds = case bounds of
        Nothing -> (minBound, pred k)
        Just (l, _) -> (l, pred k)
  let rbounds = case bounds of
        Nothing -> (succ k, maxBound)
        Just (_, r) -> (succ k, r)
  TestMap l <- arbitrarySizedMap (Just lbounds) (m `div` 2)
  TestMap r <- arbitrarySizedMap (Just rbounds) (m `div` 2)
  pure $ TestMap (Branch (k, v) l r)

-- | Custom type which is not instance of 'Ord'
data Custom = A | B | C
  deriving (Show, Eq)

instance Arbitrary Custom where
  arbitrary = elements [A, B, C]

