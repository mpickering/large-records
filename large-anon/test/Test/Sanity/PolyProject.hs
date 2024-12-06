{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE FlexibleContexts    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Test.Sanity.PolyProject (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Record.Anon
import Data.Record.Anon.Advanced (Record)
import qualified Data.Record.Anon.Advanced as Anon
import Control.Monad.State

tests :: TestTree
tests = testGroup "Test.Sanity.PolyProject" [
      testGroup "Isomorphic poly projections" [
          testCase "id"      test_id
        , testCase "reorder" test_reorder
        , testCase "merge"   test_merge
        , testCase "poly"   test_poly
        , testCase "poly_get" test_poly_get
        , testCase "test_poly_2" test_poly_2
        , testCase "test_poly_rule" test_poly_bool
        ]
    , testGroup "General lenses" [
          testCase "lens" test_lens
        ]
    ]

{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record I [ "a" := Bool, "b" := Char, "c" := Int ]
recordA =
      Anon.insert #a (I True)
    $ Anon.insert #b (I 'a')
    $ Anon.insert #c (I 1)
    $ Anon.empty

recordA' :: Record I [ "b" := Char, "a" := Bool, "c" := Int ]
recordA' =
      Anon.insert #b (I 'a')
    $ Anon.insert #a (I True)
    $ Anon.insert #c (I 1)
    $ Anon.empty

recordWithMerge :: Record I (Merge '[ "a" := Bool ] [ "b" := Char, "c" := Int ])
recordWithMerge =
    Anon.merge
      ( Anon.insert #a (I True)
      $ Anon.empty
      )
      ( Anon.insert #b (I 'a')
      $ Anon.insert #c (I 1)
      $ Anon.empty
      )

recordB :: Record I [ "c" := Int, "b" := Char ]
recordB =
      Anon.insert #c (I 1)
    $ Anon.insert #b (I 'a')
    $ Anon.empty



{-------------------------------------------------------------------------------
  Tests for isomorphic projections
-------------------------------------------------------------------------------}

test_id :: Assertion
test_id = assertEqual "" recordA $ Anon.projectK recordA

test_reorder :: Assertion
test_reorder = assertEqual "" recordA' $ Anon.projectK recordA

test_merge :: Assertion
test_merge = assertEqual "" recordA $ Anon.projectK recordWithMerge

-- Polykinded projection

data V a

type family F a where
  F (V a) = a

newtype Wrapper a = Wrapper { getF :: F a }

recordB' :: Record Wrapper [ "c" := V Int, "b" := V Char ]
recordB' =
      Anon.insert #c (Wrapper 1)
    $ Anon.insert #b (Wrapper 'a')
    $ Anon.empty

recordBM' :: Record (StateT Int I :.: Wrapper) [ "c" := V Int, "b" := V Int ]
recordBM' =
      Anon.insert #c (Comp (Wrapper <$> get))
    $ Anon.insert #b (Comp (Wrapper <$> get))
    $ Anon.empty


type family Eval a :: Row * where
  Eval '[] = '[]
  Eval (n := t ': xs) = (n := F t) ': (Eval xs)

removeWrapper :: SubRowK Wrapper I fs (Eval fs) => Record Wrapper fs -> Record I (Eval fs)
removeWrapper = Anon.projectK

removeWrapper2 :: SubRowK (m :.: Wrapper) (m :.: I) fs (Eval fs) => Record (m :.: Wrapper) fs -> Record (m :.: I) (Eval fs)
removeWrapper2 = Anon.projectK


type Rule (env :: Row *) a = Record I env -> a

combineRule :: (SubRow (Merge env1 env2) env1
               , SubRow (Merge env1 env2) env2) =>
              Rule env1 a -> (a -> Rule env2 b) ->  Rule (Merge env1 env2) b
combineRule f k = \e -> k (f (Anon.project e)) (Anon.project e)

r1 :: Rule '[ "a" := Int ] Int
r1 = \env -> unI (Anon.get #a env) + 100

r2 :: Int -> Rule '[ "a" := Int, "b" := Int ] Int
r2 b = \env -> unI (Anon.get #a env) + unI (Anon.get #b env) + b

r :: Rule ('[ "a" := Int, "b" := Int ]) Int
r = projectRule (combineRule r1 r2)

projectRule :: SubRow env env' => Rule env' a -> Rule env a
projectRule f env = f (Anon.project env)

-- Coercible (Wrapper (V Int)) (I Int), therefore should be able to project recordB'

test_poly :: Assertion
test_poly = assertEqual "" recordB $ Anon.projectK recordB'

test_poly_get :: Assertion
test_poly_get = assertEqual "" (I 1) (Anon.get #c (removeWrapper recordB'))

test_poly_2 = assertEqual "" (I (I 1)) $ evalStateT (unComp (Anon.get #c (removeWrapper2 recordBM'))) 1


test_poly_bool = assertEqual "" 112 $ r (Anon.insert #a (I 1) $ Anon.insert #b (I 10) $ Anon.empty)



{-------------------------------------------------------------------------------
  Test for more general lenses
-------------------------------------------------------------------------------}

test_lens :: Assertion
test_lens = do
    let (getter, setter) = Anon.lensK recordA
    assertEqual "get" recordB $
      getter
    assertEqual "set" (Anon.set #c (I 2) recordA) $
      setter (Anon.set #c (I 2) recordB)
