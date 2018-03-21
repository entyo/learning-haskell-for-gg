module Shapes (Point, Shape, area, nudge, baseCircle, baseRect) where
-- Export constructors of Point and Shape:
-- module Shapes (Point(..), Shape(..), area, nudge, baseCircle, baseRect) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rect Point Point deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ (2 :: Integer)
area (Rect (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- It's ok to use "as pattern": p1
-- area (Rectangle p1@(Point x1 y1) p2@(Point x2 y2)) = (abs (x2 - x1) * abs (y2 - y1))

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x + dx) (y + dy)) r
nudge (Rect (Point x1 y1) (Point x2 y2)) dx dy = Rect (Point (x1 + dx) (y1 + dy)) (Point (x2 + dx) (y2 + dy))

-- Creates it at (0, 0)
baseRect :: Float -> Float -> Shape
baseRect w h = Rect (Point 0 0) (Point w h)

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)
