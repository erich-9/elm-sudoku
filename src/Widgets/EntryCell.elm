module Widgets.EntryCell (..) where

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import MVC.Widget as Widget
import MVC.Widget exposing (..)
import Signal exposing (Address)
import String exposing (concat)
import Sudoku exposing (..)
import Utilities exposing (..)


type alias News =
  Widget.News Action Notification


type Action
  = MarkImpossible Bool
  | SetFixed Bool


type Notification
  = Clicked


type alias Model =
  { value : Maybe Value, fixed : Bool, impossible : Bool }


type alias InitData =
  { value : Maybe Value, fix : Bool }


init : InitData -> Model
init { value, fix } =
  { value = value, fixed = fix, impossible = False }


view : Address News -> Model -> Html
view addr m =
  let
    attributes =
      [ classStr, onClick addr (fromNotification Clicked) ]

    classStr =
      [ entryType, if' m.fixed " fixed" "", if' m.impossible " impossible" "" ]
        |> (concat >> class)

    ( entryType, content ) =
      let
        f v =
          ( "entry", [ (text << toString) (v + 1) ] )
      in
        mapWithDefault ( "empty", [] ) f m.value
  in
    div attributes content


update : Action -> Model -> Model
update act m =
  case act of
    MarkImpossible enable ->
      { m | impossible = enable }

    SetFixed enable ->
      { m | fixed = enable }
