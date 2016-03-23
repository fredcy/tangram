module Update (Action(..), update) where

import Effects exposing (Effects)
import Model exposing (..)


type Action
  = NoOp
  | UpdateDimensions ( Int, Int )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    UpdateDimensions dimensions ->
      ( { model | dimensions = dimensions }, Effects.none )

    NoOp ->
      ( model, Effects.none )
