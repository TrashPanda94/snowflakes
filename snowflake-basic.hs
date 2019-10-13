--
-- Generate and output a snowflake image as an SVG file.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)
import Data.List (intercalate)
import Data.Function (on)

--------------------------------------
--               SVG                --
--------------------------------------

--
-- Represents a SVG/XML attribute with a key and value.
--
data Attribute = Attribute String String

--
-- Represents a SVG/XML node with a name, attributes, and child nodes.
--
data Node = Node String [Attribute] [Node]

--
-- Create a new attribute from a showable value
--
attr :: (Show a) => String -> a -> Attribute
attr key value = Attribute key (show value)

--
-- Create an SVG root element with a xmlns attribute and width and height.
--
svg :: Int -> Int -> [Node] -> Node
svg width height = Node
  "svg"
  [Attribute "xmlns" "http://www.w3.org/2000/svg", attr "width" size, attr "height" size]

--
-- Create an SVG group element.
--
group :: [Attribute] -> [Node] -> Node
group = Node "g"

--
-- Converts an attribute into a SVG string.
--
showAttribute :: Attribute -> String
showAttribute (Attribute key val) =
    key ++ ('=' : show val)

--
-- Creates an svg tag with a given name and list of attributes.
--
-- Examples:
-- rect = Node "rect" [(Attribute "x" "10"),(Attribute "y" "0")] []
-- showNode rect == "<rect x=\"10\" y=\"0\" />"
--
-- showNode (Node "g" [] [rect]) ==
--   "<g><rect x=\"10\" y=\"0\" /></g>"
--
showNode :: Node -> String
showNode (Node name attributes children) =
    "<" ++ name ++ " " ++ (intercalate " " (map showAttribute attributes)) ++ suffix children
    where
        suffix [] = " />"
        suffix children = ">\n" ++ (intercalate "\n" (map showNode children)) ++ "\n</" ++ name ++ ">"

--------------------------------------
--             Vector2              --
--------------------------------------

--
-- Data type for point in 2D space.
--
data Vector2 = Vector2 Float Float
  deriving (Show)

--
-- Add two vectors together.
--
offset :: Vector2 -> Vector2 -> Vector2
offset (Vector2 ax ay) (Vector2 bx by) = Vector2 (ax + bx) (ay + by)

--
-- Scale a vector by a scalar.
--
scale :: Vector2 -> Float -> Vector2
scale (Vector2 x y) scalar = Vector2 (scalar * x) (scalar * y)

--
-- Slice an integer in two.
--
half :: (Integral a) => a -> Float
half x = x `divToFloat` 2
  where divToFloat = (/) `on` fromIntegral

--
-- Returns a point representing the center of a rectangle of some width and height.
--
center :: (Integral a) => a -> a -> Vector2
center width height = Vector2 (half width) (half height)

--
-- Gets a point on a unit circle at a certain angle.
--
unitCircle :: Float -> Vector2
unitCircle angle = Vector2 (cos angle) (sin angle)

--
-- Gets a list of points needed to create a polygon with n sides.
-- Points are located on a unit circle and can be scaled and transformed after.
--
polygon :: Int -> [Vector2]
polygon num_sides = [unitCircle (theta (fromIntegral i)) | i <- [0..num_sides]]
  where
    theta :: Float -> Float
    theta index = index * 2 * pi / (fromIntegral num_sides)

--------------------------------------
--              Lines               --
--------------------------------------

--
-- Creates an SVG `line` node from the first vector to the second vector
--
line :: Vector2 -> Vector2 -> Node
line (Vector2 x1 y1) (Vector2 x2 y2) =
  Node "line" [attr "x1" x1, attr "y1" y1, attr "x2" x2, attr "y2" y2] []

--
-- Returns a list of `line` nodes that connect the points given
--
buildLines :: [Vector2] -> [Node]
buildLines points = [line one two | (one, two) <- zip points (tail points)]

--
-- Returns two `line` nodes that extend from origin to the given offset,
-- mirrored on the Y axis.
--
spurs :: Vector2 -> Vector2 -> [Node]
spurs origin (Vector2 offsetX offsetY) = [line origin (offset origin e) | e <- endpoints]
  where endpoints = [Vector2 (offsetX * x) offsetY | x <- [-1, 1]]

--------------------------------------
--           Snowflakes             --
--------------------------------------

randomPolygons :: Int -> Vector2 -> [Float] -> [Float] -> [Node]
randomPolygons _ _ [] _ = []
randomPolygons num_sides center (radius:rest) (r:s:rand) =
  polyNode a ++ polyNode b ++ randomPolygons num_sides center rest rand
  where
    a = randomBetween (radius-5) (radius+5) r
    b = a + (randomBetween 3 10 s)
    scaleAndMove :: Float -> Vector2 -> Vector2
    scaleAndMove radius point = offset center (scale point radius)
    polyNode :: Int -> [Node]
    polyNode radius = buildLines (map (scaleAndMove (fromIntegral radius)) (polygon num_sides))


snowflake :: Int -> Int -> [Float] -> Node
snowflake size num_sides (r:s:rest) =
  svg size size back_layer
  where
    polygonRadii = map (* (half size)) [0.15, 0.365, 0.42, 0.47]
    moveToCenter = offset (center size size)
    back_layer = buildLines [moveToCenter (scale p 2.0) | p <- polygon num_sides]

--------------------------------------

--
-- The width and height of the image being generated.
--
size :: Int
size = 384

--
-- Other constants used during the generation of the image
--
split_low = 120 :: Int
split_penalty = 1.5 :: Float
fill_prob = 0.25 :: Float

--------------------------------------
--             Random               --
--------------------------------------

--
-- Generate and return a list of random floating point numbers between 0 and 1.
--
randomList :: Int -> [Float]
randomList seed = rl_helper (mkStdGen seed)
  where
    rl_helper :: StdGen -> [Float]
    rl_helper g = fst vg : rl_helper (snd vg)
      where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomBetween :: (Real a) => a -> a -> Float -> Int
randomBetween low high x = round ((realToFrac (high - low) * x) + realToFrac low)

--
-- Generate the tag for a rectangle with random color.  Replace the
-- implementation of this function so that it generates all of the tags
-- needed for a piece of random Mondrian art.
--
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:rest: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian _ _ 0 _ rvals = (rvals, "")
mondrian _ _ _ 0 rvals = (rvals, "")
mondrian x y w h (r:s:rest)
  | w > size `div` 2 &&
    h > size `div` 2 = b_split x y w h (r:s:rest)
  | w > size `div` 2  = h_split x y w h (r:s:rest)
  | h > size `div` 2 = v_split x y w h (r:s:rest)
  | hs && vs           = b_split x y w h rest
  | hs                 = h_split x y w h rest
  | vs                 = v_split x y w h rest
  | otherwise = (s:rest, "<rect x=\"" ++ (show x) ++
                         "\" y=\"" ++ (show y) ++
                         "\" width=\"" ++ (show w) ++
                         "\" height=\"" ++ (show h) ++
                         "\" stroke=\"black\" stroke-width=\"3\" fill=\"" ++
                         (randomColor r) ++
                         "\" />\n")
  where
    rand1 = randomInt split_low (round (fromIntegral w * split_penalty)) r
    hs = if rand1 < w then True else False
    rand2 = randomInt split_low (round (fromIntegral h * split_penalty)) s
    vs = if rand2 < h then True else False

--
--  Split the region both horizontally and vertically
--
b_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
b_split x y w h (r:s:rest) = (rest4, s1 ++ s2 ++ s3 ++ s4)
  where
    h_rand = randomInt 33 67 r
    v_rand = randomInt 33 67 s
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y lw th rest
    (rest2, s2) = mondrian (x + lw) y rw th rest1
    (rest3, s3) = mondrian x (y + th) lw bh rest2
    (rest4, s4) = mondrian (x + lw) (y + th) rw bh rest3

--
--  Split the region horizontally so that we get two that are side by side
--
h_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
h_split x y w h (r:rest) = (rest2, s1 ++ s2)
  where
    h_rand = randomInt 33 67 r
    lw = (fromIntegral w * h_rand `div` 100)
    rw = (fromIntegral w - lw)
    (rest1, s1) = mondrian x y lw h rest
    (rest2, s2) = mondrian (x + lw) y rw h rest1

--
--  Split the region vertically so that we get one on top the other
--
v_split :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
v_split x y w h (r:rest) = (rest2, s1 ++ s2)
  where
    v_rand = randomInt 33 67 r
    th = (fromIntegral h * v_rand `div` 100)
    bh = (fromIntegral h - th)
    (rest1, s1) = mondrian x y w th rest
    (rest2, s2) = mondrian x (y + th) w bh rest1

--
--  Select the random fill color for the region
--
randomColor :: Float -> String
randomColor rval
  | rval < 1.0 * fill_prob / 3.0 = "red"
  | rval < 2.0 * fill_prob / 3.0 = "skyblue"
  | rval < 3.0 * fill_prob / 3.0 = "yellow"
  | otherwise                    = "white"

--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<svg xmlns=\"http://www.w3.org/2000/svg\" " ++
               "width=\"" ++ (show size) ++
               "\" height=\"" ++ (show size) ++ "\">"
      image = snd (mondrian 0 0 size size randomValues)
      suffix = "</svg>"

  writeFile "snowflake.svg" (prefix ++ image ++ suffix)
