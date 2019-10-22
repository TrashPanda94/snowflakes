import System.IO
import Control.Monad (replicateM)
--import System.Random (randomRIO, StdGen, randomR, mkStdGen)
import Data.List (intercalate)
import Data.Function (on)

data Attribute = Attribute String String
    deriving Show
-- Main> Attribute "a" "b"
-- Attribute "a" "b"

data Node = Node String [Attribute] [Node]
    deriving Show 
-- Main> Node "str" [Attribute "a" "b"] []
-- Node "str" [Attribute "a" "b"] []

attr :: (Show a) => String -> a -> Attribute
attr key value = Attribute key (show value)
-- *Main> attr "keyString" 1
-- Attribute "keyString" "1"

showAttribute :: Attribute -> String
showAttribute (Attribute key val) =
    key ++ ('=' : show val)
-- *Main> showAttribute (Attribute "key" "val")
-- "key=\"val\""

data Vector2 = Vector2 Float Float
  deriving (Show)
-- Main> a = Vector2 1.2 1.2
-- Main> show a
-- "Vector2 1.2 1.2"

offset :: Vector2 -> Vector2 -> Vector2
offset (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax + bx) (ay + by)
-- Main> offset (Vector2 1.1 1.2) (Vector2 1.3 1.4)
-- Vector2 2.4 2.6
-- *Main> offset (Vector2 1.1 (-1.2)) (Vector2 (-1.3) (-1.4))
-- Vector2 (-0.19999993) (-2.6)
-- No positive offset allowed


scale :: Vector2 -> Float -> Vector2
scale (Vector2 x y) scalar = Vector2 (scalar * x) (scalar * y)
-- *Main> scale (Vector2 1.1 1.2) 0
-- Vector2 0.0 0.0
-- *Main> scale (Vector2 1.1 1.2) 1
-- Vector2 1.1 1.2
-- *Main> scale (Vector2 1.1 1.2) 0.9
-- Vector2 0.99 1.08

divToFloat :: (Integral a) => a -> a -> Float
divToFloat = (/) `on` fromIntegral
-- Main> divToFloat 3 2
-- 1.5

half :: (Integral a) => a -> Float
half x = x `divToFloat` 2
-- Main> half 3
-- 1.5

center :: (Integral a) => a -> a -> Vector2
center width height = Vector2 (half width) (half height)
-- *Main> center 4 4
-- Vector2 2.0 2.0

unitCircle :: Float -> Vector2
unitCircle angle = Vector2 (cos angle) (sin angle)
-- *Main> unitCircle 0.5
-- Vector2 0.87758255 0.47942555
-- *Main> unitCircle 0
-- Vector2 1.0 0.0
-- *Main> unitCircle (pi/6)
-- Vector2 0.8660254 0.5
-- *Main> (sqrt 3)/2
-- 0.8660254037844386


polygonPoints :: Int -> [Vector2]
polygonPoints num_sides = [unitCircle (theta (fromIntegral i)) | i <- [1..num_sides]]
  where
    theta :: Float -> Float
    theta index = index * 2 * pi / (fromIntegral num_sides)
-- Main> polygonPoints 3
-- [Vector2 (-0.50000006) 0.8660254,Vector2 (-0.4999999) (-0.86602545),Vector2 1.0 1.7484555e-7]
-- points at theta -pi/3, -2pi/3, 0

svgPoints :: [Vector2] -> Attribute
svgPoints (h:t) = Attribute "points" (showPoint h ++ showPoints t)
  where
    showPoint (Vector2 x y) = show x ++ "," ++ show y
    showPoints [] = ""
    showPoints (h:t) =  " " ++ showPoint h ++ showPoints t


polyline :: [Vector2] -> Node
polyline points = Node "polyline" [svgPoints points] []

polygon :: [Vector2] -> Node
polygon points = Node "polygon" [svgPoints points] []


path :: [Vector2] -> Node
path points = Node "path" [(Attribute "d" (d "M" points))] []
  where
    d command [] = " Z"
    d command ((Vector2 x y):rest) = command ++ show x ++ " " ++ show y ++ d " L" rest

line :: Vector2 -> Vector2 -> Node
line (Vector2 x1 y1) (Vector2 x2 y2) =
  Node "line" [attr "x1" x1, attr "y1" y1, attr "x2" x2, attr "y2" y2] []


buildLines :: [Vector2] -> [Node]
buildLines points = [line one two | (one, two) <- zip points (tail points)]


spurs :: Vector2 -> Vector2 -> Node
spurs origin (Vector2 offsetX offsetY) = polyline points
  where
    endpoint x = offset origin (Vector2 (offsetX * x) offsetY)
    points = [endpoint (-1), origin, endpoint 1]

rotate :: (Show a, Num a) => a -> Vector2 -> Attribute
rotate deg (Vector2 centerX centerY) =
  Attribute "transform" ("rotate(" ++ show deg ++ "," ++ show centerX ++ "," ++ show centerY ++ ")")


