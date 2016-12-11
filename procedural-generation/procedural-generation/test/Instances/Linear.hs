module Instances.Linear where

import Test.QuickCheck.Arbitrary

import Linear (V2(V2), V3(V3))

instance (Arbitrary a) => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (V3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary
