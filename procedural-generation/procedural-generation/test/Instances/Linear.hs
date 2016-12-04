module Instances.Linear where

import Test.QuickCheck.Arbitrary

import Linear (V3(V3))

instance (Arbitrary a) => Arbitrary (V3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
