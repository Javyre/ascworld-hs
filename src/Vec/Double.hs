module Vec.Double
    ( Vec3(..)
    , Point3(..)
    , Vec4(..)
    , Mat4(..)
    , Ray3(..)
    , Transform(..)
    , Polygon(..)
    ) where

import qualified Vec
-- import Vec hiding (Vec3, Ray3, Point3, Transform)

type Vec3      = Vec.Vec3 Double
type Ray3      = Vec.Ray3 Double

type Vec4      = Vec.Vec4 Double
type Mat4      = Vec.Mat4 Double

type Point3    = Vec.Point3    Double
type Transform = Vec.Transform Double

type Polygon   = Vec.Polygon Double
