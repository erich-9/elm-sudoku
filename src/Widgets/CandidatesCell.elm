module Widgets.CandidatesCell (..) where

import Array exposing (Array)
import Html exposing (..)
import MVC.Widget as Widget
import MVC.Widget exposing (..)
import Signal exposing (Address)
import Sudoku exposing (..)
import Utilities exposing (..)
import Widgets.EntryCell as EC


type alias News =
  Widget.News Action Notification


type Action
  = SetCandidates Values
  | ToggleEntry Value
  | UpdateEntry Value EC.Action


type Notification
  = Clicked Value


type alias Model =
  { entries : Array EC.Model, n : Int }


type alias InitData =
  { candidates : Values, n : Int }


init : InitData -> Model
init { candidates, n } =
  let
    f v =
      { value = Just v, fixed = False, impossible = not (member v candidates) }
  in
    { entries = Array.initialize (n * n) f, n = n }


view : Address News -> Model -> Html
view addr m =
  let
    row r =
      tr [] (List.map (viewEntry addr) r)

    groupedEntries =
      listGroupBy m.n (Array.toIndexedList m.entries)
  in
    table [] (List.map row groupedEntries)


viewEntry : Address News -> ( Value, EC.Model ) -> Html
viewEntry addr ( v, ecm ) =
  let
    childAddr =
      forwardToChild addr (UpdateEntry v) (entryNotificationHandler v)
  in
    td [] [ EC.view childAddr ecm ]


update : Action -> Model -> Model
update act m =
  case act of
    SetCandidates candidates ->
      (\v ecm -> { ecm | impossible = not (member v candidates) })
        |> updateEntries m

    ToggleEntry v ->
      (\ecm -> { ecm | impossible = not ecm.impossible })
        |> updateEntry m v

    UpdateEntry v ecAct ->
      updateEntry m v (EC.update ecAct)


updateEntry : Model -> Value -> (EC.Model -> EC.Model) -> Model
updateEntry m v f =
  { m | entries = arrayUpdate v f m.entries }


updateEntries : Model -> (Value -> EC.Model -> EC.Model) -> Model
updateEntries m f =
  { m | entries = Array.indexedMap f m.entries }


entryNotificationHandler : Value -> EC.Notification -> News
entryNotificationHandler v ntf =
  case ntf of
    EC.Clicked ->
      fromNotification (Clicked v)
