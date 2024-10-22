module Core.Algebra where

import Core.Space

negativeOne :: Float
negativeOne = -1.0

-- (LinearSpace v) =>
data Point v = Point v
    deriving (Show, Eq, Ord)

type Point2D = Point Vector2D
type Point3D = Point Vector3D

makePoint2D :: Float -> Float -> Point2D
makePoint2D x y = Point (Vector2D x y)

chooseMin :: Ord a => (a, b) -> (a, b) -> (a, b)
chooseMin (est, val) (est', val') = if est < est' then (est, val) else (est', val')

findMinFoldable :: (Foldable t, Ord a) => t (a, b) -> b
findMinFoldable = snd . foldr1 chooseMin

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = (take n xs) : (chunksOf n (drop n xs))

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : (merge xs (y:ys)) else y : (merge (x:xs) ys)

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = let n = length xs in
          let half = div n 2 in
          let left = sort (take half xs) in
          let right = sort (drop half xs) in
          merge left right


class LinearSpace vector where
    nullVector :: vector
    add :: vector -> vector -> vector
    scale :: Float -> vector -> vector

    sub :: vector -> vector -> vector
    sub lhs rhs = add lhs (scale negativeOne rhs)

translate :: LinearSpace vector => vector -> Point vector -> Point vector
translate v (Point r) = Point (add v r)

fromto :: LinearSpace vector => Point vector -> Point vector -> vector
fromto (Point r) (Point r') = sub r' r


class LinearSpace vector => EuclideanSpace vector where
    scalarProduct :: vector -> vector -> Float

    proj :: vector -> vector -> vector
    proj base v = scale (scalarProduct base v / scalarProduct base base) base

    squaredNorm :: vector -> Float
    squaredNorm v = scalarProduct v v

    norm :: vector -> Float
    norm = sqrt . squaredNorm


instance LinearSpace Vector2D where
    nullVector = Vector2D 0.0 0.0

    add (Vector2D x y) (Vector2D x' y') = Vector2D (x + x') (y + y')
    scale lambda (Vector2D x y) = Vector2D (lambda * x) (lambda * y)

instance EuclideanSpace Vector2D where
    scalarProduct (Vector2D x y) (Vector2D x' y') = x * x' + y * y'

instance LinearSpace Vector3D where
    nullVector = Vector3D 0.0 0.0 0.0

    add (Vector3D x y z) (Vector3D x' y' z') = Vector3D (x + x') (y + y') (z + z')
    scale lambda (Vector3D x y z) = Vector3D (lambda * x) (lambda * y) (lambda * z)

instance EuclideanSpace Vector3D where
    scalarProduct (Vector3D x y z) (Vector3D x' y' z') = x * x' + y * y' + z * z'
