-- Vector2 Test
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
