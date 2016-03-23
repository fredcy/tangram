module View (..) where

import Color exposing (Color)
import Colors exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Graphics.Input exposing (clickable)
import Model exposing (..)
import Update exposing (Action(..))

view : Signal.Address Action -> Model -> Element
view address model =
  let
    form =
      group
        [ formFromPiece model.pieces.bigTriangleS
        , formFromPiece model.pieces.bigTriangleW
        , formFromPiece model.pieces.mediumTriangle
        , formFromPiece model.pieces.smallTriangleSE
        , formFromPiece model.pieces.smallTriangleN
        , formFromPiece model.pieces.parallelogram
        , formFromPiece model.pieces.square
        ]
        |> scale (scalingFactor model.dimensions)

    ( width, height ) =
      model.dimensions
  in
    collage width height [ form ]
      |> clickable (Signal.message address Click)


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
