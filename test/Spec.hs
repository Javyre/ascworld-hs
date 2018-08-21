{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Gen

import Control.Monad
import Data.Maybe

import qualified Vec as V
import Vec hiding (Vec3, Vec4, Mat4, Transform)
import Vec.Double

-- | Like '/=', but prints a counterexample when it fails.
-- infix 4 /==
(/==) :: (Eq a, Show a) => a -> a -> Property
x /== y =
    counterexample (show x ++ interpret res ++ show y) res
    where
        res = x /= y
        interpret True  = " /= "
        interpret False = " == "


arbDouble = elements $
    ([ 0.0, 1.0, 5.0, 2.5, -4.5, -10.0 ])

instance Arbitrary Vec3 where
    arbitrary = V.Vec3 <$> arbDouble <*> arbDouble <*> arbDouble

instance Arbitrary Vec4 where
    arbitrary = V.Vec4 <$> arbDouble <*> arbDouble <*> arbDouble <*> arbDouble

instance Arbitrary Mat4 where
    arbitrary = V.Mat4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Transform where
    arbitrary = V.Transform <$> arbitrary


main :: IO()
main = hspec $ do
    describe "Vec.Transform" $ do
        prop "is associative with itself" $
            \t0 t1 t2 ->
                let a = (t0 `apply`  t1) `apply` t2  :: Transform
                    b =  t0 `apply` (t1  `apply` t2) :: Transform
                in
                    a === b


        prop "is not associative with a vec and itself" $
            \v t0 t1 ->
                let a = (v `apply`  t0) `apply` t1  :: Vec3
                    b =  v `apply` (t0  `apply` t1) :: Vec3
                in
                    a /== b

    describe "Vec.intersect" $ do
        prop "should work on hard-coded test" $
            let o :: V.Point3 Double
                v :: V.Vec3 Double
                ray :: V.Ray3 Double
                poly :: V.Polygon Double

                o = V.Point3 1.0 1.0 0.0
                v = V.Vec3   0.0 0.0 (-1.0)
                ray = V.Ray3 { origin=o, Vec.vector=v }

                poly = Triangle
                    (Point3 0.0 0.0 (-4.0))
                    (Point3 3.0 0.0 (-4.0))
                    (Point3 3.0 3.0 (-4.0))
            in
                isJust $ intersect ray poly

