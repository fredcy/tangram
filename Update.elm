module Update (Action(..), update) where

import Effects exposing (Effects)
import Model exposing (..)
import Time exposing (Time)


type Action
  = NoOp
  | UpdateDimensions ( Int, Int )
  | Click
  | Tick Time


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action |> Debug.log "action" of
    UpdateDimensions dimensions ->
      ( { model | dimensions = dimensions }, Effects.none )

    Click ->
      ( { model | animation = AnimationStarting 1000 }
      , Effects.tick Tick
      )

    Tick time ->
      case model.animation of
        AnimationIdle ->
          ( model, Effects.none ) |> Debug.log "error: got Tick while idle"

        AnimationStarting duration ->
          let
            animation =
              AnimationActive
                { duration = duration
                , start = time
                }
          in
            ( { model | animation = animation }, Effects.tick Tick )

        AnimationActive { duration, start } ->
          let
            fraction =
              (time - start) / duration
          in
            if fraction >= 1.0 then
              ( { model | animation = AnimationIdle } |> animate 1
              , Effects.none
              )
            else
              ( model |> animate fraction
              , Effects.tick Tick
              )

    NoOp ->
      ( model, Effects.none )


animate : Float -> Model -> Model
animate fraction model =
  let
    square =
      model.square

    square' =
      { square | rotation = fraction * (degrees 90) }
  in
    { model | square = square' }
