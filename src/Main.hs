module Main where

import System.Console.ANSI
import Camera

import Vec
import Scene
import Data.Colour.SRGB.Linear
import Control.Concurrent

scene = Scene { camera=cam, objects=objs }
    where cam = createCamera (sw, sh) (0.25, 0.5)
              `setEyePos` (Point3 ex ey 45.0)
              -- `apply` translate (Vec3 0 0 45.0)

          sw = 130
          sh = 40
          ex = (fromIntegral sw) * 0.25 / 2.0
          ey = (fromIntegral sh) * 0.5 / 2.0

          objs =
              [ Object (rgb 1.0 0.0 1.0) [ Triangle (Point3 10.0 10.0 (-20.0))
                                                    (Point3 20.0 10.0 (-20.0))
                                                    (Point3 20.0 20.0 (-20.0))
                                         , Triangle (Point3 10.0 10.0 (-20.0))
                                                    (Point3 10.0 20.0 (-20.0))
                                                    (Point3 20.0 20.0 (-20.0))
                                         , Triangle (Point3 10.0 10.0 (-20.0))
                                                    (Point3 10.0 20.0 (-40.0))
                                                    (Point3 20.0 20.0 (-40.0))
                                         , Triangle (Point3 10.0 10.0 (-40.0))
                                                    (Point3 10.0 20.0 (-40.0))
                                                    (Point3 20.0 20.0 (-20.0))
                                         ]
              -- , Object (rgb 1.0 1.0 0) [ Triangle (Point3 00.0 10.0 (-20.0))
              --                                     (Point3 20.0 00.0 (-20.0))
              --                                     (Point3 20.0 10.0 (-20.0))
              --                                     ]
              ]

rotationT = pivot (Point3 20.0 20.0 (-20.0)) (Vec3 0.0 1.0 0.0) (-pi*2.0 / (360.0 / 2))
    -- `apply` pivot (Point3 15.0 15.0 (-10.0)) (Vec3 0.0 0.0 1.0) (-pi*2.0 / (360.0 / 2))

main :: IO ()
main = do
    putStrLn "hello world"
    let loop scene = do {
        let nscene = scene { objects=(map (\o -> o `apply` rotationT) (objects scene)) }
            in do
                display (render nscene)
                threadDelay (truncate $ 1000000.0/100.0)
                loop nscene
    }
        in loop scene

