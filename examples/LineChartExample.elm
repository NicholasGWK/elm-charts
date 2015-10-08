import LineChart exposing (chart, point)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)


data = [(10,10), (20,20), (20,-30)]

dataAsPoints data=
  List.map (\tuple -> point tuple)  data

main =
  collage 200 200 (chart (dataAsPoints data) red)
