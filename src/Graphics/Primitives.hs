module Graphics.Primitives where

import qualified Core.Algebra as CA
import qualified Core.Color as CC
import qualified Core.Image as CI
import qualified Core.Space as CS

type ImageTransform = CI.Image -> CI.Image

run :: ImageTransform -> CI.Image
run f = f (CI.staticColor CC.black)

type AbstractShape2D = CA.Point2D -> Bool

plane :: AbstractShape2D
plane _ = True

halfPlane :: CA.Point2D -> CA.Point2D -> AbstractShape2D
halfPlane p q = let v = CA.fromto p q in
                \pt -> CS.pseudoScalarProduct v (CA.fromto p pt) >= 0

intersect :: AbstractShape2D -> AbstractShape2D -> AbstractShape2D
intersect f g = \pt -> and [(f pt), (g pt)]

union :: AbstractShape2D -> AbstractShape2D -> AbstractShape2D
union f g = \pt -> or [(f pt), (g pt)]

line :: CA.Point2D -> CA.Point2D -> AbstractShape2D
line p q = intersect (halfPlane p q) (halfPlane q p)

polygon :: [CA.Point2D] -> AbstractShape2D
polygon pts = let n = length pts in
              let nextPt = take n . drop 1 . cycle $ pts in
              let halfPlanes = map (uncurry halfPlane) (zip pts nextPt) in
              foldr intersect plane halfPlanes

paint :: AbstractShape2D -> CC.Color -> ImageTransform
paint shape c (CI.Image img) = CI.Image img'
    where img' :: CA.Point2D -> CC.Color
          img' pt = if shape pt then c else img pt
