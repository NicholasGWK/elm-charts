module LineChart (chart) where

{-| Line Chart

Takes a list of tuples representing x,y and makes a line with dots on the actual points. Lots of work
To be done, but is a POC

#Definition
@docs chart


-}
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)

import Maybe exposing (..)


type alias Dataset = List (Float, Float)

{-| Creates a line chart

  chart [(0,0), (10,10)] == "Line with two points at 0,0 and 10,10"
-}
chart: Dataset -> Color -> Int -> Int -> Element
chart myData dataColor height width =
   collage height width (lines (scaleAndMove myData height width) dataColor  :: dots (scaleAndMove myData height width) dataColor)



scaleAndMove: Dataset -> Int -> Int -> Dataset
scaleAndMove data height width =
  moveToOrigin (scaleData data height width) height width

moveToOrigin: Dataset -> Int -> Int -> Dataset
moveToOrigin data height width =
  List.map (\tuple -> ((fst tuple) - (toFloat width), (snd tuple) - (toFloat height) / 2)) data

scaleData: Dataset -> Int -> Int -> Dataset
scaleData data height width =
  List.map (\tuple -> ((fst tuple * ((toFloat width) / (findDomain data)), (snd tuple * ((toFloat height) / findRange data))) )) data

lines: Dataset -> Color -> Form
lines dataset dataColor =
  path dataset
    |> traced (solid dataColor)

dots: Dataset -> Color -> List Form
dots dataset dataColor =
  List.map (\tuple -> move (fst tuple, snd tuple) ( filled dataColor (circle 3))) dataset


tupleRange: List (Float,Float) -> ((Float,Float) -> Float) -> Float -> Float
tupleRange dataset fn max =
  List.map fn dataset |> List.maximum |> withDefault max

findDomain: List (Float,Float) -> Float
findDomain dataset =
  tupleRange dataset fst 100

findRange: List (Float,Float) -> Float
findRange dataset =
  tupleRange dataset snd 100
