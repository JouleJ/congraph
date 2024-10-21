module Core.Metric where

import Core.Algebra
import Core.Color

class MetricSpace point where
    distance :: point -> point -> Float

instance (EuclideanSpace vector) => MetricSpace (Point vector) where
    distance p q = norm (fromto p q)

instance MetricSpace Color where
    distance (Color r g b) (Color r' g' b') = abs(r - r') + abs(g - g') + abs(b - b')
