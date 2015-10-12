import LineChart exposing (chart)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import List exposing (..)
import Maybe exposing (..)

data = [(10,10), (20,20), (25,25)]


main =
  chart data red 500 500
