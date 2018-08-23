{-#
    LANGUAGE MultiParamTypeClasses
#-}

module Camera
    ( Camera(..)
    , Eye(..)
    , Screen(..)
    , createCamera
    , getCorner
    , getEyePos
    , setEyePos
    , getRay
    ) where

import Vec.Double
import Data.Functor.Identity
import qualified Vec as V
import Vec hiding (Vec3, Ray3, Point3, Transform)

data Camera = Camera Eye Screen
    deriving (Show)

data Eye = Eye
    { relativePos :: Point3 -- position relative to the top-left of the screen
    }
    deriving (Show)

data Screen = Screen
    { width  :: Int
    , height :: Int

    , cellWidth  :: Double
    , cellHeight :: Double

    , currentTransform :: Transform
    , corners :: (Point3, Point3, Point3, Point3)
        -- clockwise corner absolute positions starting from top-left

    , centers :: [[Point3]]

    }
    deriving (Show)

instance Transformable Camera Double where
    apply (Camera e s) t = Camera ne ns
        where ne = e
              ns = s
                  { corners =
                      ( getCorner s 0 `apply` t
                      , getCorner s 1 `apply` t
                      , getCorner s 2 `apply` t
                      , getCorner s 3 `apply` t
                      )
                  , currentTransform =
                      currentTransform s `apply` t
                  , centers =
                      map (map $ \v -> v `apply` t) $ centers s
                  }

createCamera (w, h) (cw, ch) = Camera e s
    where
        wf = fromIntegral w
        hf = fromIntegral h

        e = Eye { relativePos = V.Point3 (wf * cw / 2.0) (hf * ch / 2.0) 1.0 }
        s = Screen
            { width = w
            , height = h
            , cellWidth = cw
            , cellHeight = ch
            , currentTransform = scale 1
            , corners =
                ( V.Point3 0.0       0.0       0.0
                , V.Point3 (wf * cw) 0.0       0.0
                , V.Point3 (wf * cw) (hf * ch) 0.0
                , V.Point3 0.0       (hf * ch) 0.0
                )
            , centers =
                [ [ V.Point3
                        (fromIntegral x*cw - cw/2)
                        (fromIntegral y*ch - ch/2)
                        0.0
                  | x <- [1 .. w] ] | y <- [1 .. h] ]
            }

-- get the coords of a corner of the screen (0-indexed)
getCorner :: Screen -> Int -> Point3
getCorner Screen { corners = (c, _, _, _) } 0 = c
getCorner Screen { corners = (_, c, _, _) } 1 = c
getCorner Screen { corners = (_, _, c, _) } 2 = c
getCorner Screen { corners = (_, _, _, c) } 3 = c


getEyePos :: Camera -> Point3
getEyePos (Camera e s) = relativePos e `apply` currentTransform s

setEyePos :: Camera -> Point3 -> Camera
setEyePos (Camera e s) pos = Camera (e { relativePos = pos }) s


getRay :: Camera -> (Int, Int) -> (Ray3)
getRay (Camera e s) (x, y)
    = V.Ray3 { origin=start, vector=dir }
    where
        c   = centers s !! y !! x
        eye = getEyePos (Camera e s)

        start = c
        dir   = point3ToVec3 $ c -^ eye


