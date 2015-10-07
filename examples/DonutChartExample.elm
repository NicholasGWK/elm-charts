import DonutChart exposing (chart)
import Graphics.Collage exposing (collage)
import Color exposing (..)
testdata = [1,1,1,1]

colors = [ red , blue , yellow , green ]

main =
  collage 200 200 (chart colors testdata 40 80)
