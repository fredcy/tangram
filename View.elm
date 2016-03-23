module View (..) where

import Color exposing (Color)
import Colors exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Model exposing (..)


view : Model -> Form
view model =
  group
    [ formFromPiece model.bigTriangleS
    , formFromPiece model.bigTriangleW
    , formFromPiece model.mediumTriangle
    , formFromPiece model.smallTriangleSE
    , formFromPiece model.square
    , formFromPiece model.parallelogram
    , formFromPiece model.smallTriangleN
    ]
    |> scale (scalingFactor model.dimensions)


formFromPiece : Piece -> Form
formFromPiece piece =
  filledForm piece.color piece.points
    |> move piece.position
    |> rotate piece.rotation
    |> scale piece.scale


scalingFactor : ( Int, Int ) -> Float
scalingFactor ( width, height ) =
  (min (toFloat width) (toFloat height)) / 3


filledForm : Color -> List ( Float, Float ) -> Form
filledForm color points =
  let
    linestyle =
      { defaultLine
        | width = separation
        , color = white
        , join = Graphics.Collage.Smooth
      }

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


displayCentered : ( Int, Int ) -> Form -> Element
displayCentered ( width, height ) form =
  collage width height [ form ]
