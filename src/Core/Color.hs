module Core.Color where

data Color = Color { getRedComponent :: Float, getGreenComponent :: Float, getBlueComponent :: Float }
    deriving (Eq, Ord, Show)

black :: Color
black = Color 0.0 0.0 0.0

white :: Color
white = Color 1.0 1.0 1.0

red :: Color
red = Color 1.0 0.0 0.0

green :: Color
green = Color 0.0 1.0 0.0

blue :: Color
blue = Color 0.0 0.0 1.0

yellow :: Color
yellow = Color (2.0 / 3.0) (1.0 / 3.0) 0.0

brightYellow :: Color
brightYellow = Color 1.0 1.0 0.0

magenta :: Color
magenta = Color (2.0 / 3.0) 0.0 (2.0 / 3.0)

cyan :: Color
cyan = Color 0.0 (2.0 / 3.0) (2.0 / 3.0)
