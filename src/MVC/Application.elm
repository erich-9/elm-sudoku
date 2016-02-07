module MVC.Application (MVC, initState, run) where

import MVC.Widget as Widget
import Signal exposing (Address, Mailbox)
import Utilities exposing (..)


type alias MVC mwModel mwAction mwNotification externalNotification out =
  { mwInitModel : mwModel
  , mwView : Address (Widget.News mwAction mwNotification) -> mwModel -> out
  , mwUpdate : mwAction -> mwModel -> mwModel
  , externalEvents : List (Signal externalNotification)
  , updateExternal : externalNotification -> mwModel -> mwModel
  }


type alias State mwModel mwNotification =
  ( mwModel, List mwNotification )


type News mwAction mwNotification externalNotification
  = Internal (Widget.News mwAction mwNotification)
  | External externalNotification


run : MVC mwM mwA mwN extN out -> Signal (State mwM mwN) -> Signal out
run mvc state =
  Signal.map (mvc.mwView mailbox.address << fst) state


initState : MVC mwM mwA mwN extN out -> Signal (State mwM mwN)
initState { mwInitModel, mwView, mwUpdate, externalEvents, updateExternal } =
  let
    f event ( m, _ ) =
      case event of
        Internal mwNews ->
          ( updateInternal mwUpdate mwNews.actions m, mwNews.notifications )

        External externalNtf ->
          ( updateExternal externalNtf m, [] )
  in
    Signal.foldp f ( mwInitModel, [] ) (events externalEvents)


events : List (Signal extN) -> Signal (News mwA mwN extN)
events externalEvents =
  Signal.map Internal mailbox.signal
    :: List.map (Signal.map External) externalEvents
    |> Signal.mergeMany


mailbox : Mailbox (Widget.News mwA mwN)
mailbox =
  Signal.mailbox { actions = [], notifications = [] }


updateInternal : (mwA -> mwM -> mwM) -> List mwA -> mwM -> mwM
updateInternal mwUpdate mwActions mwModel =
  List.foldl mwUpdate mwModel mwActions
