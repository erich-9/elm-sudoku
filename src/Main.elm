module Main (main) where

import Html exposing (Html)
import MVC.Application exposing (MVC, initState, run)
import Signal exposing (Signal, Mailbox)
import Sudoku exposing (..)
import Utilities exposing (..)
import Widgets.SudokuGame as MW


main : Signal Html
main =
  run mvc state


state : Signal ( MW.Model, List MW.Notification )
state =
  initState mvc


mvc : MVC MW.Model MW.Action MW.Notification ExternalNotification Html
mvc =
  { mwInitModel = MW.init initData
  , mwView = MW.view
  , mwUpdate = MW.update
  , externalEvents = externalEvents
  , updateExternal = updateExternal
  }


initData : MW.InitData
initData =
  { sudoku = deserialize initialSudoku
  , fixInitialNonEmptyEntries = True
  , showCandidates = Maybe.withDefault False storedState
  }


type ExternalNotification
  = NewAvailable (Maybe SerializedSudoku)
  | SolutionAvailable (List SerializedSudoku)


externalEvents : List (Signal ExternalNotification)
externalEvents =
  [ Signal.map NewAvailable newAvailable
  , Signal.map SolutionAvailable solutionAvailable
  ]


updateExternal : ExternalNotification -> MW.Model -> MW.Model
updateExternal ntf m =
  case ntf of
    NewAvailable mssdk ->
      let
        g x =
          MW.update (MW.Load (deserialize x)) m
      in
        mapWithDefault m g mssdk

    SolutionAvailable ssdks ->
      let
        g x =
          MW.update (MW.LoadSolution x) m
      in
        case ssdks of
          x :: xs ->
            let
              t =
                if' (List.isEmpty xs) Solution MultipleSolutions
            in
              g (t (deserialize x))

          _ ->
            g NoSolution



-- Incoming Ports


port storedState : Maybe Bool
port initialSudoku : SerializedSudoku
port newAvailable : Signal (Maybe SerializedSudoku)
port solutionAvailable : Signal (List SerializedSudoku)



-- Outgoing Ports


port requestSaveState : Signal Bool
port requestSaveState =
  Signal.map (\( m, _ ) -> m.showCandidates) state


port requestNew : Signal ()
port requestNew =
  let
    f ( _, n ) =
      if List.member MW.RequestNew n then
        Just ()
      else
        Nothing
  in
    Signal.filterMap f () state


port requestSolution : Signal (Maybe SerializedSudoku)
port requestSolution =
  let
    f ( m, n ) =
      if List.member MW.RequestSolution n then
        Just (Just (serialize m.sudoku))
      else
        Nothing
  in
    Signal.filterMap f Nothing state
