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
import Debug exposing (..)
import Maybe exposing (..)


type alias Dataset = List (Float, Float)

{-| Creates a line chart

  chart [(0,0), (10,10)] == "Line with two points at 0,0 and 10,10"
-}
chart: Dataset -> Color -> Int -> Int -> Element
chart data dataColor height width =
   collage height width (append (lines (scaleAndMove height width data) dataColor  :: dots (scaleAndMove height width data) dataColor) (axes width height))

scaleAndMove: Int -> Int -> Dataset -> Dataset
scaleAndMove height width data =
  scaleData height width data |> moveToOrigin height width

moveToOrigin: Int -> Int -> Dataset -> Dataset
moveToOrigin height width data =
  List.map (\tuple -> ((fst tuple) - (toFloat width  / 2), (snd tuple) - (toFloat height) / 2)) data

scaleData: Int -> Int -> Dataset -> Dataset
scaleData height width data =
  let scaledX = (\tuple -> fst tuple * (toFloat width) / (findDomain data))
      scaledY = (\tuple -> snd tuple * (toFloat height) / (findRange data))
  in

  List.map (\ tuple -> (scaledX tuple, scaledY tuple)) (log "data" data) |> log "scale"

lines: Dataset -> Color -> Form
lines dataset dataColor =
  path dataset
    |> traced (solid dataColor)

dots: Dataset -> Color -> List Form
dots dataset dataColor =
  List.map (\tuple -> move (fst tuple, snd tuple) ( filled dataColor (circle 3))) dataset


tupleRange: Dataset -> ((Float,Float) -> Float) -> Float -> Float
tupleRange dataset fn max =
  List.map fn dataset |> List.maximum |> withDefault max

findDomain: Dataset -> Float
findDomain dataset =
  tupleRange dataset fst 100

findRange: Dataset -> Float
findRange dataset =
  tupleRange dataset snd 100


baseAxes: Int -> Dataset
baseAxes vert =
  List.foldr (\num prev -> (toFloat num, 0) :: (toFloat num,1) :: prev) [] [0..vert] |> log "base"

axes: Int -> Int -> List Form
axes width height =
  let tickLines = scaleAndMove width height (baseAxes 5)
      appendLine = (\line prev -> (lines line black) :: prev)
  in
  List.foldr appendLine []  (log "tick" tickLines)
