{-# LANGUAGE StandaloneDeriving, GeneralizedNewtypeDeriving #-}

import Unify
import Data.Maybe
import Control.Applicative
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.HUnit

import Data.List

--deriving instance Arbitrary Name
instance Arbitrary Name where
    arbitrary = N <$> elements ["a", "b", "c", "d", "e"]

genType 0 = TypeVar <$> arbitrary
genType n = oneof [genType 0, Fun <$> subGen <*> subGen]
  where
    subGen = genType (n - 1)

instance Arbitrary Type where
  arbitrary = sized genType

main = defaultMain tests
tests
  = [ testGroup "Properties"
      [ testProperty "unify_var" prop_unify_var
      , testProperty "unify with self" prop_unify_self 
      ]
    , testGroup "Testcases"
	[ testCase "sym" test_sym1
	, testCase "compose1" test_compose1
	, testCase "compose2" test_compose2
	, testCase "compose3" test_compose3
	, testCase "compose4" test_compose4
	, testCase "compose5" test_compose5
	]
    ]

a = tVar "a"
b = tVar "b"
c = tVar "c"
d = tVar "d"
e = tVar "e"

prop_unify_var t = not (N "a" `elem` freeVars t) ==> successful $ unify (tVar "a") t
  where types = (t :: Type)

prop_unify_self t = successful $ unify t t
  where types = (t :: Type)

test_sym1 =
  unify (fun b c) a @=?
  unify a (fun b c)

baseUnifySubst t1 t2 = do
  let (Right s) = unify t1 t2
  let st1 = doSubst s t1
  let st2 = doSubst s t2
  (st1, st2)

-- For inspecting results.
unifySubst t1 t2 = if st1 == st2 then Just st1 else Nothing
  where
    (st1, st2) = baseUnifySubst t1 t2

testUnify t1 t2 = st1 @?= st2
  where
    (st1, st2) = baseUnifySubst t1 t2

unifyShouldFail t1 t2 = do
  case (unify t1 t2) of
    Left _ -> assert True
    Right s ->
      assertFailure $ "Unexpected substitution: " ++ show s ++ "; after substitution, the terms give: " ++ (show $ unifySubst t1 t2)
  

test_compose1 = testUnify (fun a (fun b c)) (fun (fun c d) e)
test_compose2 = testUnify (fun a (fun b c)) (fun (fun b c) e)
test_compose3 = testUnify (fun a (fun b c)) (fun (fun b c) a)
test_compose4 = unifyShouldFail (fun a (fun b c)) (fun (fun a c) a) -- Should fail
test_compose5 = unifyShouldFail (fun a (fun b c)) (fun (fun b c) b)
-- vim: set sw=2:
