--
-- Generate and output a snowflake image as an SVG file.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)
import Data.Char (isDigit)
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
svg :: Int -> Int -> [Attribute] -> [Node] -> Node
svg width height attrs = Node
  "svg"
  ([Attribute "xmlns" "http://www.w3.org/2000/svg", Attribute "viewBox" ("0 0 " ++ show width ++ " " ++ show height)] ++ attrs)

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
-- Divide an integer by an integer and get a float
--
divToFloat :: (Integral a) => a -> a -> Float
divToFloat = (/) `on` fromIntegral

--
-- Slice an integer in two.
--
half :: (Integral a) => a -> Float
half x = x `divToFloat` 2

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
polygonPoints :: Int -> [Vector2]
polygonPoints num_sides = [unitCircle (theta (fromIntegral i)) | i <- [1..num_sides]]
  where
    theta :: Float -> Float
    theta index = index * 2 * pi / (fromIntegral num_sides)

--------------------------------------
--              Lines               --
--------------------------------------

--
-- Creates a string of SVG points for a polygon or polyline.
--
svgPoints :: [Vector2] -> Attribute
svgPoints (h:t) = Attribute "points" (showPoint h ++ showPoints t)
  where
    showPoint (Vector2 x y) = show x ++ "," ++ show y
    showPoints [] = ""
    showPoints (h:t) =  " " ++ showPoint h ++ showPoints t

--
-- Creates a SVG `polyline` node that draws a line connecting all the points.
--
polyline :: [Vector2] -> Node
polyline points = Node "polyline" [Attribute "fill" "none", svgPoints points] []

--
-- Creates a SVG `polygon` node that draws a self-closing line connecting all the points
--
polygon :: [Vector2] -> Node
polygon points = Node "polygon" [Attribute "fill" "none", svgPoints points] []

--
-- Creates an SVG `path` node connecting all the vectors given.
-- Paths are represented by a series of "commands":
-- "Mx y" representing moving a "pen" to the given coordinates.
-- "Lx y" representing drawing a line with the pen from the previous set of coordinates.
--
path :: [Vector2] -> Node
path points = Node "path" [(Attribute "d" (d "M" points))] []
  where
    d command [] = " Z"
    d command ((Vector2 x y):rest) = command ++ show x ++ " " ++ show y ++ d " L" rest

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
-- Returns `polyline` node representing two lines that extend from origin to the given offset,
-- mirrored on the Y axis.
--
spurs :: Vector2 -> Vector2 -> Node
spurs origin (Vector2 offsetX offsetY) = polyline points
  where
    endpoint x = offset origin (Vector2 (offsetX * x) offsetY)
    points = [endpoint (-1), origin, endpoint 1]

--
-- Creates a transform attribute that rotates by `deg` degrees
-- around in the given origin.
--
rotate :: (Show a, Num a) => a -> Vector2 -> Attribute
rotate deg (Vector2 centerX centerY) =
  Attribute "transform" ("rotate(" ++ show deg ++ "," ++ show centerX ++ "," ++ show centerY ++ ")")

--------------------------------------
--           Snowflakes             --
--------------------------------------

--
-- Generates a polygon node for a snowflake
--
snowflakePolygon :: Int -> Vector2 -> Float -> Node
snowflakePolygon num_sides center radius =
  polygon [offset center (scale point radius) | point <- polygonPoints num_sides]

--
-- Generates a spur node for a snowflake
--
snowflakeSpur :: Vector2 -> (Float, Float) -> (Float, Float) -> (Float, Float) -> IO Node
snowflakeSpur (Vector2 centerX centerY) start length spread = do
  sStart <- randomRIO start
  sLength <- randomRIO length
  sSpread <- randomRIO spread
  return (spurs (Vector2 centerX sStart) (Vector2 sSpread (sLength * (-1))))

--
-- Generates a spoke node for a snowflake, along with its spurs
--
snowflakeSpoke :: Vector2 -> Float -> IO Node
snowflakeSpoke (Vector2 centerX centerY) deg = do
  a <- spur (145,150) (50,55) (40,50)
  b <- spur (130,135) (35,40) (20,30)
  c <- spur (90,100) (25,35) (15,20)
  d <- spur (65,70) (10,20) (5,10)
  e <- spur (30,35) (15,20) (5,7)
  return (group [rotate deg center] [mainLine, a, b, c, d, e])
  where
    center = (Vector2 centerX centerY)
    spur = snowflakeSpur center
    mainLine = line (Vector2 centerX 0) center

--
-- Generates multiple polygons for the background of a snowflake.
-- Polygon sizes are slightly randomized.
--
randomPolygons :: Int -> Vector2 -> IO Node
randomPolygons num_sides (Vector2 centerX centerY) = do
  (a,b) <- randomPolygon 30
  (c,d) <- randomPolygon 70
  (e,f) <- randomPolygon 90
  (g,h) <- randomPolygon 110
  return (group [] [a,b,c,d,e,f,g,h])
  where
    center = Vector2 centerX centerY
    randomPolygon radius = do
      a <- randomRIO (radius-5, radius+5 :: Float)
      b <- randomRIO (3, 10 :: Float)
      return (polyNode a, polyNode (a + b))
      where polyNode = snowflakePolygon num_sides center

--
-- Generates an SVG image of a snowflake.
-- The snowflake is represented using multiple polygons and paths
-- that are randomly generated.
-- The number of branches coming out of the snowflake can be configured by `num_sides`.
--
snowflake :: Int -> String -> IO Node
snowflake num_sides color = do
  back_layer <- randomPolygons num_sides centerPoint
  spoke_groups <- spokes spokeAngles
  return (svg size size attrs (back_layer : spoke_groups))
  where
    size = 384
    attrs = [Attribute "fill" "none", Attribute "stroke" color, attr "stroke-width" 2]
    centerPoint = center size size
    spokeTurnAmount = 360 `divToFloat` num_sides
    spokeAngles = [90 + (fromIntegral i * spokeTurnAmount) | i <- [1..num_sides]]
    spokes :: [Float] -> IO [Node]
    spokes [] = do
      return []
    spokes (h:t) = do
      spoke <- snowflakeSpoke centerPoint h
      mapped <- spokes t
      return (spoke : mapped)

--------------------------------------
--             Prompts              --
--------------------------------------

--
-- Displays a question to the user and validates the result.
--
prompt :: String -> (String -> Bool) -> IO String
prompt question valid = do
  putStrLn(question)
  reply <- getLine
  if (valid reply) 
    then return reply
    else do
      putStrLn("Invalid entry. Please try again!")
      prompt question valid

--
-- Checks that a color is one of a few valid SVG colors.
--      
isValidColor :: String -> Bool
isValidColor color = 
  elem color 
  ["indianred","lightcoral","salmon","crimson","red"
  ,"darkred","pink","lightpink","hotpink","deeppink","mediumvioletred","palevioletred"
  ,"lightsalmon","coral","tomato","orangered","darkorange","orange"
  ,"gold","yellow","moccasin","peachpuff","palegoldenrod","khaki"
  ,"plum","violet","fuchsia","mediumorchid","blueviolet","purple"
  ,"chatreuse","limegreen","mediumseagreen","forestgreen","darkgreen","darkcyan"
  ,"aqua","steelblue","deepskyblue","blue","navy","turquoise"
  ,"burlywood","sandybrown","darkgoldenrod","saddlebrown","brown","goldenrod"
  ,"white","honeydew","aliceblue","seashell","beige","mistyrose"
  ,"gainsboro","silver","gray","lightslategray","darkslategrey", "gray", "black"]

--
-- Checks that every item in a list is True.
--  
every :: [Bool] -> Bool
every lst = foldr (&&) True lst

--
-- Checks that a string represents a number.
--
isInt :: String -> Bool
isInt str = every (map isDigit str)

--------------------------------------

--
-- The main program which generates and outputs snowflake.svg
--
main :: IO ()
main = do

  -- Number of sides of snowflake
  num_sides_str <- prompt 
    "How many sides does your snowflake have? (Enter a number at least 3)"
    (\ x -> every [isInt x, (read x :: Int) > 2])
  -- Converts String to Int
  let num_sides = read num_sides_str :: Int

  -- Color of snowflake
  color <- prompt "What color is your snowflake?" isValidColor
  
  node <- snowflake num_sides color
  writeFile "snowflake.svg" (showNode node)