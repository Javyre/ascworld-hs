{-#
    LANGUAGE ScopedTypeVariables
           , FlexibleInstances
           , MultiParamTypeClasses
#-}

module Scene
    ( Object(..)
    , Scene(..)
    , RenderedScreen(..)
    , render
    , display
    ) where

import Vec
import Camera
import Data.Colour.SRGB.Linear
import Data.Colour as RS
import Data.Maybe
import System.Console.ANSI
import qualified System.Console.ANSI.Codes as ANSI

data (Fractional a) => Object a = Object (Colour Float) [Polygon a]
    deriving (Show)

data Scene = Scene
    { camera  :: Camera
    , objects :: [Object Double]
    }
    deriving (Show)

data RenderedScreen = RScreen [[Colour Float]]
    deriving (Show)

instance (Fractional a) => Transformable (Object a) a where
    apply (Object clr pls) t = Object clr (map (\p -> p `apply` t) pls)

enumerate l = zip ([0..(length l)]) l


testRay :: forall a. (Floating a, Real a, Ord a) => Ray3 a -> [Object a] -> Maybe (Colour Float)
testRay ray objs
    = getCol <$> -- Get colour of intersected obj

        -- reduce to closest intersection out of the list
        -- :: Maybe (Object, Point3, a <dist of point>)
        (foldl (reduce) Nothing $

            -- find intersection for each object (discard Nothings)
            -- :: [(Object, Point3, a <dist of point>)]
            catMaybes $ map (findInter) objs)

    where
        getCol :: (Object a, Point3 a, a) -> Colour Float
        getCol ((Object clr _), _, d) = RS.darken (realToFrac $ 1-(d*0.9/30.0) :: Float) clr
        -- getCol :: Maybe (Object, Point3 a) -> Maybe (Colour Float)
        -- getCol (Nothing)     = Nothing
        -- getCol (Just ((Object clr _), p)) = Just clr


        reduce :: Maybe (Object a, Point3 a, a)
               -> (Object a, Point3 a, a)
               -> Maybe (Object a, Point3 a, a)

        reduce (Nothing) p = Just p
        reduce (Just a@(_, _, d0)) b@(_, _, d1)
            | d0 > d1   = Just b
            | otherwise = Just a


        dist :: Point3 a -> a
        dist p = (origin ray) `distance` p

        findInter :: Object a -> Maybe (Object a, Point3 a, a)
        findInter obj@(Object clr polys) =
            let
                inters :: [(Point3 a, a)]
                inters = map (\e -> (e, dist e)) is
                    where is = catMaybes $ map (intersect ray) polys

                -- (inters :: [Point3])

                -- reduce :: Maybe Point3 -> Point3 -> Maybe Point3
                reduce' (Nothing) b@(_, _) = Just b
                reduce' (Just a@(_, ad))  b@(_, bd)
                    | ad > bd         = Just a
                    | otherwise       = Just b
            in
                -- find closest intersection for object (if any)
                (\(p, d) -> (obj, p, d)) <$> foldl (reduce') Nothing inters

render :: Scene -> RenderedScreen
render s = RScreen rows
    where
        rows = map
            (\(y, row) ->
                map
                    (\(x, cell) -> maybe (rgb 0 0 0) (id) $ testRay (getRay cam (x,y)) objs)
                    $ enumerate row)
            $ enumerate ctrs

        cam@(Camera eye screen) = camera s

        ctrs = centers screen
        objs = objects s

display :: RenderedScreen -> IO ()
display (RScreen rows) = do
    putStr $ ANSI.hideCursorCode ++ (ANSI.setCursorPositionCode 0 0)
    mapM_
        (\r -> do
            mapM_ (\c -> do
                setSGR [SetRGBColor Background c]
                putStr " "
                )
                r
            putStrLn ""
            )
        rows
    showCursor

