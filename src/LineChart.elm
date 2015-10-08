module LineChart (chart, point) where

{-| Line Chart

Takes a list of points and makes a line with dots on the actual points. Lots of work
To be done, but is a POC

#Definition
@docs chart


#Helpers
@docs point

-}
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)
import Window exposing (..)

type alias Point =
  {
  x: Float,
  y: Float
  }

type alias Dataset = List Point

{-| Converts a  Float tuple into a point

    point (0,10) == {x:0, y:10}
-}
point: (Float,Float) -> Point
point (x,y) =
  {
  x =  x,
  y =  y
  }

{-| Creates a line chart of sorts

    chart [{x: 1, y: 1}, {x:10, y: 10}] red = "red line graph"

-}
chart: Dataset -> Color -> List Form
chart myData dataColor =
   lines myData dataColor  :: dots myData dataColor


points2tuples: List Point -> List (Float,Float)
points2tuples dataset =
  List.map (\point -> (point.x, point.y)) dataset

lines: Dataset -> Color -> Form
lines dataset dataColor=
  traced (solid dataColor) (path (points2tuples dataset))

dots: Dataset -> Color -> List Form
dots dataset dataColor =
  List.map (\point -> (move (point.x, point.y)) (filled dataColor (circle 3))) dataset
