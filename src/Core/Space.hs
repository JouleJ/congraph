module Core.Space where

data Vector2D = Vector2D Float Float
    deriving (Show, Eq, Ord)

data Vector3D = Vector3D Float Float Float
    deriving (Show, Eq, Ord)

pseudoScalarProduct :: Vector2D -> Vector2D -> Float
pseudoScalarProduct (Vector2D x y) (Vector2D x' y') = x * y' - x' * y
