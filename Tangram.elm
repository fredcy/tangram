module Main (..) where

import Graphics.Element exposing (Element)
import Window
import Model exposing (Model, init)
import Signal.Extra
import View exposing (view)
import Update exposing (..)
import Task
import Effects exposing (Effects)
import Start exposing (start, Config, App)


main : Signal Element
main =
  app.output


dimensionSignal : Signal Action
dimensionSignal =
  Signal.map UpdateDimensions Window.dimensions


app =
  start
    { init = ( Model.init, sendInitial )
    , update = update
    , view = view
    , inputs = [ firstResize, dimensionSignal ]
    }


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks



{- All of the following is needed just to get the initial window dimensions. -}


appStartMailbox : Signal.Mailbox ()
appStartMailbox =
  Signal.mailbox ()


{-| Get the initial value of the `resizes` signal by sampling it on the signal of a
mailbox that we use just for this purpose.
-}
firstResize : Signal Action
firstResize =
  Signal.sampleOn appStartMailbox.signal dimensionSignal


{-| An Effect that sends a message to a special mailbox just so we have a signal
that triggers the sampleOn above.
-}
sendInitial : Effects Action
sendInitial =
  Signal.send appStartMailbox.address ()
    |> Task.map (always NoOp)
    |> Effects.task
