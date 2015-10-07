module LineChart where

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

point: (Float,Float) -> Point
point (x,y) =
  {
  x =  x,
  y =  y
  }


myData =
  [point(0,0), point(10,10), point(20,-10)]


main =
  Signal.map view Window.dimensions


view (width,height) =
  collage width height ( lines myData :: dots myData)



type alias Dataset = List Point


points2tuples: List Point -> List (Float,Float)
points2tuples dataset =
  List.map (\point -> (point.x, point.y)) dataset


lines dataset =
  traced (solid red) (path (points2tuples dataset))

dots: Dataset -> List Form
dots dataset =
  List.map (\point -> (move (point.x, point.y)) (filled red (circle 5))) dataset
