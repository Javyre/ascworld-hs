module Main where

import System.Console.ANSI
import qualified System.Console.ANSI.Codes as ANSI
import Camera

import Vec
import Scene
import Data.Colour.SRGB.Linear
import Control.Concurrent

testCam = cam
    -- `apply` translate (Vec3 1.0 1.0 0)
    -- `apply` rotate (Vec3 0 0 1.0) (-pi*2.0 / 4.0)
    -- `apply` translate (Vec3 (-1.0) (-1.0) 0)

    -- `apply` translate (Vec3 (-1.0) (-1.0) 0)
    `apply` pivot (Point3 1.0 1.0 0) (Vec3 0 0 1.0) (-pi*2.0 / 4.0)
    -- `apply` translate (Vec3 0 0 $ -1.0)
    where cam = createCamera (2, 2) (1.0, 1.0)

scene = Scene { camera=cam, objects=objs }
    where cam = createCamera (120, 35) (0.5, 1.0)
              `apply` translate (Vec3 0 0 9.0)

          objs =
              [ Object (rgb 1.0 0.0 0) [ Triangle (Point3 0.0 0.0   (-3.0))
                                                (Point3 20.0 0.0  (-3.0))
                                                (Point3 20.0 20.0 (-3.0))
                                                ]
              ]

rotationT = pivot (Point3 10.0 10.0 0) (Vec3 0 0 1.0) (-pi*2.0 / 64.0)
    `apply` pivot (Point3 10.0 10.0 0) (Vec3 0 1.0 0) (-pi*2.0 / (64.0 * 2.0))

main :: IO ()
main = do
    let rs = render scene
        in display rs

    putStrLn "hello world"
    putStrLn $ "screen corners = " ++ (show $ corners s)
    putStrLn $ "screen (1,1) = " ++ (show $ centers s !! 1 !! 1)
    putStrLn $ "abs eye pos  = " ++ (show $ getEyePos testCam)

    putStrLn $ "ray (1,1)    = " ++ (show $ (getRay testCam (1, 1)) `point` 1.0)

    let loop scene = do {
        let nscene = scene { objects=(map (\o -> o `apply` rotationT) (objects scene)) }
            in do
                display (render nscene)
                threadDelay (truncate $ 1000000.0/60.0)
                loop nscene
    }
        in loop scene

    where
        Camera e s = testCam
