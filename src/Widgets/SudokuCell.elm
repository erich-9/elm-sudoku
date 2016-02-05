module Widgets.SudokuCell (..) where

import Html exposing (Html)
import MVC.Widget as Widget
import MVC.Widget exposing (..)
import Signal exposing (Address)
import Sudoku exposing (..)
import Utilities exposing (..)
import Widgets.CandidatesCell as CC
import Widgets.EntryCell as EC


type alias News =
  Widget.News Action Notification


type Action
  = ECAction EC.Action
  | CCAction CC.Action
  | SetCandidates Values


type Notification
  = ECNotification EC.Notification
  | CCNotification CC.Notification


type Model
  = ECModel EC.Model
  | CCModel CC.Model


type alias InitData =
  { cell : Cell, n : Int, fixNonEmpty : Bool, showCandidates : Bool }


init : InitData -> Model
init { cell, n, fixNonEmpty, showCandidates } =
  case cell of
    Left value ->
      (ECModel << EC.init) { value = Just value, fix = fixNonEmpty }

    Right candidates ->
      if showCandidates then
        (CCModel << CC.init)
          { candidates = candidates, n = n }
      else
        (ECModel << EC.init) { value = Nothing, fix = False }


initWithNonFixedEntryCell : Cell -> Model
initWithNonFixedEntryCell cell =
  (ECModel << EC.init) { value = fromLeft cell, fix = False }


tryToInitWithCandidateCell : Cell -> Int -> Bool -> Model
tryToInitWithCandidateCell cell n showCandidates =
  init
    { cell = cell, n = n, fixNonEmpty = False, showCandidates = showCandidates }


isFixed : Model -> Bool
isFixed m =
  case m of
    ECModel m ->
      m.fixed

    _ ->
      False


isUnfixed : Model -> Bool
isUnfixed m =
  case m of
    ECModel m ->
      m.value /= Nothing && not m.fixed

    _ ->
      False


view : Address News -> Model -> Html
view addr m =
  case m of
    ECModel m ->
      let
        childAddr =
          forwardToChild addr ECAction (fromNotification << ECNotification)
      in
        EC.view childAddr m

    CCModel m ->
      let
        childAddr =
          forwardToChild addr CCAction (fromNotification << CCNotification)
      in
        CC.view childAddr m


update : Action -> Model -> Model
update act model =
  case ( act, model ) of
    ( ECAction a, ECModel m ) ->
      ECModel (EC.update a m)

    ( CCAction a, CCModel m ) ->
      CCModel (CC.update a m)

    ( SetCandidates candidates, CCModel m ) ->
      CCModel (CC.update (CC.SetCandidates candidates) m)

    ( SetCandidates candidates, ECModel m ) ->
      let
        f v =
          not (member v candidates)
      in
        ECModel { m | impossible = mapWithDefault False f m.value }

    _ ->
      model
