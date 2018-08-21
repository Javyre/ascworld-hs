{-#
    LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , DefaultSignatures
           , KindSignatures
           , FlexibleInstances
#-}

module Vec
    ( Vec3(..)
    , Point3(..)
    , Vec4(..)
    , Mat4(..)
    , Ray3(..)
    , Transform(..)
    , Transformable
    , Polygon(..)

    , (*^), (/^)
    , (+^), (-^)
    , absV
    , point
    , distance

    , apply
    , rotate
    , pivot
    , scale
    , scaleP
    , translate

    , point3ToVec3
    , vec3ToPoint3

    , (.^), (..^), (^.^)

    , intersect

    ) where

import Text.Show.Functions
import Control.Applicative

data Point3 a = Point3 a a a
    deriving (Show, Eq)

data Vec3 a = Vec3 a a a
    deriving (Show, Eq)

data Vec4 a = Vec4 a a a a
    deriving (Show, Eq)


data Num a => Ray3 a = Ray3 { origin :: Point3 a, vector :: Vec3 a }
    deriving (Show)

-- data Plane = Plane ((Double, Double) -> Point3))
--     deriving (Show)

data Mat4 a = Mat4 (Vec4 a) (Vec4 a) (Vec4 a) (Vec4 a)
    deriving (Eq)

instance (Show a) => Show (Mat4 a) where
    show (Mat4 a b c d) = "Mat4:\n\t"
        ++ show a ++ "\n\t"
        ++ show b ++ "\n\t"
        ++ show c ++ "\n\t"
        ++ show d ++ ""


data Transform a = Transform (Mat4 a)
    deriving (Eq, Show)

class (Num a) => Transformable t a | t -> a where
    infixr 7 `apply`
    apply :: t -> Transform a -> t

instance (Num a) => Transformable (Point3 a) a where
    apply p t = vec4ToPoint3 $ point3ToVec4 p `apply` t

instance (Num a) => Transformable (Vec3 a) a where
    apply v t = vec4To3 $ vec3To4 v `apply` t

instance (Num a) => Transformable (Vec4 a) a where
    apply v (Transform m) = m ..^ v

instance (Num a) => Transformable (Transform a) a where
    apply (Transform m1) (Transform m2) = Transform $ m1 ^.^ m2

-- rotate along an axis (angle t)
rotate (Vec3 l m n) t = Transform $ Mat4
    (Vec4 (l*l * cm +     ct) (m*l * cm - n * st) (n*l * cm + m * st) 0)
    (Vec4 (l*m * cm + n * st) (m*m * cm +     ct) (n*m * cm - l * st) 0)
    (Vec4 (l*n * cm - m * st) (m*n * cm + l * st) (n*n * cm +     ct) 0)
    (Vec4 0 0 0 1)
    
    where cm = (1 - ct)
          ct = cos t
          st = sin t

pivot_ :: Fractional a => Point3 a -> Transform a -> Transform a
pivot_ ppoint trans =
    translate ofs
        `apply` trans
        `apply` translate (ofs *^ (-1.0))
    where
        ofs = point3ToVec3 ppoint

-- rotate along a pivot point
pivot ppoint axe t = pivot_ ppoint $ rotate axe t

-- uniformly shrink by a factor
scale f = Transform $ Mat4
    (Vec4 f 0 0 0)
    (Vec4 0 f 0 0)
    (Vec4 0 0 f 0)
    (Vec4 0 0 0 1)
    

-- scale around a pivot point
scaleP ppoint f = pivot_ ppoint $ scale f


translate (Vec3 x y z) = Transform $ Mat4
    (Vec4 1 0 0 x)
    (Vec4 0 1 0 y)
    (Vec4 0 0 1 z)
    (Vec4 0 0 0 1)

class (Applicative v) => VecOps v where
    (+^) :: Num t => v t -> v t -> v t
    (-^) :: Num t => v t -> v t -> v t

    a +^ b = liftA2 (+) a b
    a -^ b = liftA2 (-) a b

    (*^) :: Num        t => v t -> t -> v t
    (/^) :: Fractional t => v t -> t -> v t

    a *^ b = fmap ((*) b) a
    a /^ b = fmap (\a' -> a' / b) a

    (.^)  :: Num t => v t -> v t -> t
    cross :: Num t => v t -> v t -> v t

instance Functor Vec3 where
    fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Applicative Vec3 where
    pure x = Vec3 x x x
    liftA2 f (Vec3 x y z) (Vec3 x' y' z') = Vec3 (f x x') (f y y') (f z z')

instance VecOps Vec3 where
    (Vec3 x y z) .^ (Vec3 x' y' z') = x*x' + y*y' + z*z'

    cross (Vec3 x y z) (Vec3 x' y' z') = Vec3
        (y*z' - z*y')
        (z*x' - x*z')
        (x*y' - y*x')

instance Functor Point3 where
    fmap f (Point3 x y z) = Point3 (f x) (f y) (f z)

instance Applicative Point3 where
    pure x = Point3 x x x
    liftA2 f (Point3 x y z) (Point3 x' y' z') = Point3 (f x x') (f y y') (f z z')

instance VecOps Point3 where
    (Point3 x y z) .^ (Point3 x' y' z') = x*x' + y*y' + z*z'

    cross (Point3 x y z) (Point3 x' y' z') = Point3
        (y*z' - z*y')
        (z*x' - x*z')
        (x*y' - y*x')

instance Functor Vec4 where
    fmap f (Vec4 x y z w) = Vec4 (f x) (f y) (f z) (f w)

instance Applicative Vec4 where
    pure x = Vec4 x x x x
    liftA2 f (Vec4 x y z w) (Vec4 x' y' z' w') = Vec4 (f x x') (f y y') (f z z') (f w w')

instance VecOps Vec4 where
    (Vec4 x y z w) .^ (Vec4 x' y' z' w') = x*x' + y*y' + z*z' + w*w'

    cross v v' = undefined

absV :: Num a =>  Vec3 a -> Vec3 a
absV (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)

norm :: Floating a => Vec3 a -> a
norm (Vec3 x y z) = sqrt $ x^2 + y^2 + z^2

distance :: Floating a => Point3 a -> Point3 a -> a
distance (Point3 x y z) (Point3 x' y' z') = sqrt $ (x-x')^2 + (y-y')^2 + (z-z')^2

class PointFunc (t :: * -> *) where
    pfunc :: Num r => t r -> (r -> Point3 r)

    point :: Num r => t r -> r -> Point3 r
    point t a = pfunc t $ a

instance PointFunc Ray3 where
    pfunc (Ray3 { origin=origin, vector=vector })
        = \t -> origin +^ (vec3ToPoint3 $ vector *^ t)


class Indexable (t :: * -> *) where
    (!) :: t r -> Int -> r

instance Indexable Vec4 where
    (Vec4 x _ _ _) ! 0 = x
    (Vec4 _ y _ _) ! 1 = y
    (Vec4 _ _ z _) ! 2 = z
    (Vec4 _ _ _ w) ! 3 = w

column :: Mat4 a -> Int -> Vec4 a
column (Mat4 a b c d) n = Vec4 (a!n) (b!n) (c!n) (d!n)

row (Mat4 a _ _ _) 0 = a
row (Mat4 _ b _ _) 1 = b
row (Mat4 _ _ c _) 2 = c
row (Mat4 _ _ _ d) 3 = d

vec3To4 (Vec3 x y z)   = Vec4 x y z 0
vec4To3 (Vec4 x y z _) = Vec3 x y z

vec3ToPoint3 (Vec3 x y z) = Point3 x y z

point3ToVec3 (Point3 x y z) = Vec3 x y z
point3ToVec4 (Point3 x y z) = Vec4 x y z 1

vec4ToPoint3 (Vec4 x y z _) = Point3 x y z

infixr 7 .^
infixr 7 ..^
infixr 7 ^.^

-- (.^)  :: Vec4 -> Vec4 -> Double
(..^) :: Num a => Mat4 a -> Vec4 a -> Vec4 a
(^.^) :: Num a => Mat4 a -> Mat4 a -> Mat4 a

-- (Vec4 x y z w) .^ (Vec4 x' y' z' w') = x*x' + y*y' + z*z' + w*w'

(Mat4 a b c d) ..^ v = Vec4 (a .^ v) (b .^ v) (c .^ v) (d .^ v)

(Mat4 a b c d) ^.^ m = Mat4 
    (Vec4 (a .^ column m 0) (a .^ column m 1) (a .^ column m 2) (a .^ column m 3))
    (Vec4 (b .^ column m 0) (b .^ column m 1) (b .^ column m 2) (b .^ column m 3))
    (Vec4 (c .^ column m 0) (c .^ column m 1) (c .^ column m 2) (c .^ column m 3))
    (Vec4 (d .^ column m 0) (d .^ column m 1) (d .^ column m 2) (d .^ column m 3))




-- == Polygons ==

data (Fractional a) => Polygon a
    = Triangle (Point3 a) (Point3 a) (Point3 a)
    deriving (Show)

-- plane (Triangle p0 p1 p2) = Plane $ (x, y) ->

instance (Fractional a) => Transformable (Polygon a) a where
    apply (Triangle p0 p1 p2) t =
        Triangle (p0 `apply` t) (p1 `apply` t) (p2 `apply` t)

class (Fractional a, Ord a) => Intersectable t a where
    intersect :: t -> Polygon a -> Maybe (Point3 a)

instance (Fractional a, Ord a) => Intersectable (Ray3 a) a where
    -- Implementation of the Möller–Trumbore algorithm
    --    my impl relies on laziness for verifying each condition incrementally
    intersect ray (Triangle p0 p1 p2)
        | cond0 || cond1 || cond2 = Nothing
        | cond3                   = Just $ ray `point` t
        | otherwise               = Nothing
        where
            cond0 = a > (-kEpsilon) && a < kEpsilon
            cond1 = u < 0 || u > 1.0
            cond2 = v < 0 || u+v > 1.0

            cond3 = t > kEpsilon
            conds = cond0 && cond1 && cond2 && cond3

            rayO = origin ray
            rayV = vector ray
            kEpsilon  = 0.0000001

            edge1 = point3ToVec3 $ p1 -^ p0
            edge2 = point3ToVec3 $ p2 -^ p0

            h = rayV `cross` edge2
            a = edge1 .^ h -- Num

            f = 1/a -- Num
            s = point3ToVec3 $ rayO -^ p0
            u = f * (s .^ h) -- Num

            q = s `cross` edge1
            v = f * (rayV .^ q) -- Num

            t = f * (edge2 .^ q)

