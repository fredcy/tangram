module Main (..) where

import Window
import Color exposing (Color, rgb)
import Graphics.Collage exposing (Form, collage, defaultLine, filled, group, move, outlined, path, polygon, scale, solid, traced)
import Graphics.Element exposing (Element)
import Colors exposing (..)


--shapes


scalingFactor : ( Int, Int ) -> Float
scalingFactor ( width, height ) =
  (min (toFloat width) (toFloat height)) / 3


filledForm : Color -> List ( Float, Float ) -> Form
filledForm color points =
  let
    linestyle =
      { defaultLine | width = separation, color = white, join = Graphics.Collage.Smooth }

    -- Add first point to end of list so path forms a loop.
    loopedPoints =
      case List.head points of
        Nothing ->
          []

        Just h ->
          List.append points [ h ]
  in
    group
      [ polygon points |> filled color
      , path loopedPoints |> traced linestyle
      ]


separation : Float
separation =
  5.0e-2


logo : Float -> Form
logo scaleFactor =
  group
    [ bigTriangleTurquoise
    , bigTriangleGray
    , mediumTriangle
    , smallTriangleSE
    , smallTriangleN
    , square
    , parallelogram
    ]
    |> scale scaleFactor


bigTriangleTurquoise : Form
bigTriangleTurquoise =
  filledForm elmTurquoise [ ( 0, 0 ), ( -1, -1 ), ( 1, -1 ) ]


bigTriangleGray : Form
bigTriangleGray =
  filledForm elmGray [ ( 0, 0 ), ( -1, -1 ), ( -1, 1 ) ]


mediumTriangle : Form
mediumTriangle =
  filledForm elmTurquoise [ ( 1, 1 ), ( 0, 1 ), ( 1, 0 ) ]


smallTriangleSE : Form
smallTriangleSE =
  filledForm elmOrange [ ( 1, -1 ), ( 1, 0 ), ( 0.5, -0.5 ) ]


smallTriangleN : Form
smallTriangleN =
  filledForm elmOrange [ ( 0, 0 ), ( 0.5, 0.5 ), ( -0.5, 0.5 ) ]


square : Form
square =
  filledForm elmGreen [ ( 0, 0 ), ( 0.5, 0.5 ), ( 1, 0 ), ( 0.5, -0.5 ) ]


parallelogram : Form
parallelogram =
  filledForm elmGreen [ ( 0, 1 ), ( -1, 1 ), ( -0.5, 0.5 ), ( 0.5, 0.5 ) ]



--functions


displayCentered : ( Int, Int ) -> Form -> Element
displayCentered ( width, height ) form =
  collage width height [ form ]


updatedScalingFactor : Signal Float
updatedScalingFactor =
  (Signal.map scalingFactor Window.dimensions)


scaledLogo : Signal Form
scaledLogo =
  (Signal.map logo updatedScalingFactor)


main : Signal Element
main =
  Signal.map2 displayCentered Window.dimensions scaledLogo
