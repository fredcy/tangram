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
        [ formFromPiece model.tangram.bigTriangle1
        , formFromPiece model.tangram.bigTriangle2
        , formFromPiece model.tangram.mediumTriangle
        , formFromPiece model.tangram.smallTriangle1
        , formFromPiece model.tangram.smallTriangle2
        , formFromPiece model.tangram.parallelogram
        , formFromPiece model.tangram.square
        ]
        |> scale (scalingFactor model.dimensions)

    ( width, height ) =
      model.dimensions
  in
    collage width height [ form ]
      |> clickable (Signal.message address Click)


formFromPiece : Piece -> Form
formFromPiece piece =
  filledForm piece.points piece.scale piece.color
    |> move piece.position
    |> rotate piece.rotation


scalingFactor : ( Int, Int ) -> Float
scalingFactor ( width, height ) =
  (min (toFloat width) (toFloat height)) / 4


{-| Create Form with given color, vertexes, and scale. We fake an inset by
applying a path to the perimeter whose width is twice the desired inset. Note
that for that inset to be the same for all pieces we have to scale the defining
points *before* applying that path.
-}
filledForm : List ( Float, Float ) -> Float -> Color -> Form
filledForm points scaleFactor color =
  let
    linestyle =
      { defaultLine
        | width = separation
        , color = Color.black
        , join = Graphics.Collage.Smooth
        , cap = Graphics.Collage.Round
      }

    scaledPoints =
      scalePoints scaleFactor points
  in
    group
      [ scaledPoints |> polygon |> filled color
      , scaledPoints |> loopPoints |> path |> traced linestyle
      ]


separation : Float
separation =
  5.0e-2


scalePoints : Float -> List Position -> List Position
scalePoints scaleFactor pts =
  case pts of
    ( x, y ) :: tail ->
      ( x * scaleFactor, y * scaleFactor ) :: scalePoints scaleFactor tail

    [] ->
      []


{-| Add first point to end of list so path forms a closed loop.
-}
loopPoints : List Position -> List Position
loopPoints pts =
  case pts of
    hd :: tail ->
      List.append pts [ hd ]

    [] ->
      []
