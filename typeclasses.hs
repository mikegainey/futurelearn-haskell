-- type class course example

data Bright = Blue
            | Red
            deriving (Read, Show)


data Pastel = Turquoise
            | Tan
            deriving (Read, Show)

class Color a where
  dark :: a -> Bool
  lighten :: a -> a

instance Color Bright where
  dark = darkBright
  lighten = lightenBright

instance Color Pastel where
  dark = darkPastel
  lighten = lightenPastel


darkBright :: Bright -> Bool
darkBright Blue = True
darkBright Red = False

lightenBright :: Bright -> Bright
lightenBright Blue = Red
lightenBright Red = Red

darkPastel :: Pastel -> Bool
darkPastel Turquoise = True
darkPastel Tan = False

lightenPastel :: Pastel -> Pastel
lightenPastel Turquoise = Tan
lightenPastel Tan = Tan

--------------------------------------------------------------------------------
-- my own example:
-- types: Circle and Square
-- typeclass: Shape
-- functions in the typeclass: area and perimeter

data Circle = Circle {radius :: Float} deriving (Show)

data Square = Square {sidelength :: Float} deriving (Show)

class Shape a where
  area :: a -> Float
  perimeter :: a -> Float

instance Shape Circle where
  area = circleArea
  perimeter = circlePerimeter

instance Shape Square where
  area = squareArea
  perimeter = squarePerimeter

circleArea :: Circle -> Float
circleArea (Circle radius) = pi * radius ^ 2

circlePerimeter :: Circle -> Float
circlePerimeter (Circle radius) = 2 * pi * radius

squareArea :: Square -> Float
squareArea (Square sidelength) = sidelength ^ 2

squarePerimeter :: Square -> Float
squarePerimeter (Square sidelength) = sidelength * 4

