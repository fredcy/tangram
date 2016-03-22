module Main (..) where

import Window
import Color exposing (Color, rgb)
import Graphics.Collage exposing (Form, collage, defaultLine, filled, group, move, outlined, path, polygon, scale, solid, traced)
import Graphics.Element exposing (Element)


--colors


elmGreen : Color
elmGreen =
  rgb 141 215 55


elmTurquoise : Color
elmTurquoise =
  rgb 96 181 204


elmOrange : Color
elmOrange =
  rgb 239 165 0


elmGray : Color
elmGray =
  rgb 90 99 120


red : Color
red =
  rgb 255 0 0


white : Color
white =
  rgb 255 255 255



--shapes


scalingFactor : ( Int, Int ) -> Float
scalingFactor ( width, height ) =
  (min (toFloat width) (toFloat height)) / 3


filledForm : Float -> Color -> List ( Float, Float ) -> Form
filledForm scaleFactor color points =
  let
    linestyle =
      { defaultLine | width = separation, color = white, join = Graphics.Collage.Smooth }

    connect points =
      case List.head points of
        Nothing ->
          []

        Just h ->
          List.append points [ h ]
  in
    group
      [ polygon points |> filled color
      , path (connect points) |> traced linestyle
      ]


separation =
  5.0e-2


logo : Float -> Form
logo scaleFactor =
  group
    [ bigTriangleTurquoise scaleFactor
    , bigTriangleGray scaleFactor
    , mediumTriangle scaleFactor
    , smallTriangleSE scaleFactor
    , smallTriangleN scaleFactor
    , square scaleFactor
    , parallelogram scaleFactor
    ]
    |> scale scaleFactor


bigTriangleTurquoise : Float -> Form
bigTriangleTurquoise scaleFactor =
  filledForm scaleFactor elmTurquoise [ ( 0, 0 ), ( -1, -1 ), ( 1, -1 ) ]


bigTriangleGray : Float -> Form
bigTriangleGray scaleFactor =
  filledForm scaleFactor elmGray [ ( 0, 0 ), ( -1, -1 ), ( -1, 1 ) ]


mediumTriangle : Float -> Form
mediumTriangle scaleFactor =
  filledForm scaleFactor elmTurquoise [ ( 1, 1 ), ( 0, 1 ), ( 1, 0 ) ]


smallTriangleSE : Float -> Form
smallTriangleSE scaleFactor =
  filledForm scaleFactor elmOrange [ ( 1, -1 ), ( 1, 0 ), ( 0.5, -0.5 ) ]


smallTriangleN : Float -> Form
smallTriangleN scaleFactor =
  filledForm scaleFactor elmOrange [ ( 0, 0 ), ( 0.5, 0.5 ), ( -0.5, 0.5 ) ]


square : Float -> Form
square scaleFactor =
  filledForm scaleFactor elmGreen [ ( 0, 0 ), ( 0.5, 0.5 ), ( 1, 0 ), ( 0.5, -0.5 ) ]


parallelogram : Float -> Form
parallelogram scaleFactor =
  filledForm scaleFactor elmGreen [ ( 0, 1 ), ( -1, 1 ), ( -0.5, 0.5 ), ( 0.5, 0.5 ) ]



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
