module Start (..) where

import Task
import Effects exposing (Effects, Never)


type alias Config model action output =
  { init : ( model, Effects action )
  , update : action -> model -> ( model, Effects action )
  , view : Signal.Address action -> model -> output
  , inputs : List (Signal.Signal action)
  }


type alias App model output =
  { output : Signal output
  , model : Signal model
  , tasks : Signal (Task.Task Never ())
  }


start : Config modelT action output -> App modelT output
start config =
  let
    singleton : action -> List action
    singleton action =
      [ action ]

    messages : Signal.Mailbox (List action)
    messages =
      Signal.mailbox []

    address : Signal.Address action
    address =
      Signal.forwardTo messages.address singleton

    --updateStep : action -> (modelT, Effects action) -> (modelT, Effects action)
    updateStep action ( oldModel, accumulatedEffects ) =
      let
        ( newModel, additionalEffects ) =
          config.update action oldModel
      in
        ( newModel, Effects.batch [ accumulatedEffects, additionalEffects ] )

    --update : List action -> (modelT, Effects action) -> (modelT, Effects action)
    update actions ( model, _ ) =
      List.foldl updateStep ( model, Effects.none ) actions

    --inputs : Signal (List action)
    inputs =
      Signal.mergeMany (messages.signal :: List.map (Signal.map singleton) config.inputs)

    --effectsAndModel : Signal (modelT, Effects action)
    effectsAndModel =
      Signal.foldp update config.init inputs

    --model : Signal modelT
    model =
      Signal.map fst effectsAndModel
  in
    { output = Signal.map (config.view address) model
    , model = model
    , tasks = Signal.map (Effects.toTask messages.address << snd) effectsAndModel
    }
