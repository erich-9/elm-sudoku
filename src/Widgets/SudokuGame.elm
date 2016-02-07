module Widgets.SudokuGame (..) where

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (id, class, disabled)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)
import MVC.Widget as Widget
import MVC.Widget exposing (..)
import Set
import Signal exposing (Address)
import String exposing (join)
import Sudoku exposing (..)
import Utilities exposing (..)
import Widgets.CandidatesCell as CC
import Widgets.EntryCell as EC
import Widgets.SudokuCell as SC


type alias News =
  Widget.News Action Notification


type Action
  = Load Sudoku
  | LoadSolution SolutionResponse
  | Clear
  | Fix
  | Reset
  | Undo
  | Redo
  | ShowCandidates Bool
  | HandleEntryClick Pos
  | HandleCandidateClick Value Pos
  | SetUnknown Pos
  | SetValue Value Pos
  | ToggleCandidate Value Pos
  | UpdateCell Pos SC.Action


type Notification
  = RequestNew
  | RequestSolution


type alias Model =
  HistoricModel'
    { showCandidates : Bool
    , selected : Maybe ( Pos, SC.Model )
    , statusMessage : String
    }


type HistoricModel
  = HistoricModel (HistoricModel' {})


type alias Premodel =
  Premodel' {}


type alias HistoricModel' a =
  Premodel'
    { a
      | predecessor : Maybe HistoricModel
      , successor : Maybe HistoricModel
      , isProgenitor : Bool
      , clean : Bool
      , cheated : Bool
    }


type alias Premodel' a =
  { a | sudoku : Sudoku, cells : Array SC.Model }


toPremodel : Model -> Premodel
toPremodel m =
  { sudoku = m.sudoku, cells = m.cells }


toHistoricModel : Model -> HistoricModel
toHistoricModel m =
  HistoricModel
    { sudoku = m.sudoku
    , cells = m.cells
    , predecessor = m.predecessor
    , successor = m.successor
    , isProgenitor = m.isProgenitor
    , clean = m.clean
    , cheated = m.cheated
    }


type alias InitData =
  { sudoku : Sudoku, fixInitialNonEmptyEntries : Bool, showCandidates : Bool }


init : InitData -> Model
init initData =
  let
    pm =
      initPremodel initData
  in
    { sudoku = pm.sudoku
    , cells = pm.cells
    , predecessor = Nothing
    , successor = Nothing
    , showCandidates = initData.showCandidates
    , selected = Nothing
    , statusMessage = ""
    , isProgenitor = True
    , clean = True
    , cheated = False
    }


initPremodel : InitData -> Premodel
initPremodel { sudoku, fixInitialNonEmptyEntries, showCandidates } =
  let
    cells =
      Array.foldl (Array.push << sc) Array.empty sudoku.board

    sc cell =
      SC.init
        { cell = cell
        , n = sudoku.n
        , fixNonEmpty = fixInitialNonEmptyEntries
        , showCandidates = showCandidates
        }
  in
    simpleElimination { sudoku = sudoku, cells = cells }


view : Address News -> Model -> Html
view addr m =
  let
    board =
      div [ id "board" ] [ table [] (cgroups ++ rgroups) ]

    rgroups =
      List.map (tbody []) (listGroupBy m.sudoku.n rows)

    rows =
      List.map row (listGroupBy m.sudoku.n2 (Array.toIndexedList m.cells))

    row r =
      tr [] (List.map (lazy2 viewCell addr) r)

    cgroups =
      List.repeat m.sudoku.n cgroup

    cgroup =
      colgroup [] (List.repeat m.sudoku.n (col [] []))

    toolbox =
      div
        [ id "toolbox", class "button-group" ]
        [ newBtn
        , clearBtn
        , fixBtn
        , resetBtn
        , undoBtn
        , redoBtn
        , showCandidatesBtn
        , solutionBtn
        ]

    newBtn =
      button
        [ class "button"
        , onClick addr (fromNotification RequestNew)
        ]
        [ text "New" ]

    clearBtn =
      button
        [ class "button"
        , onClick addr (fromAction Clear)
        ]
        [ text "Clear" ]

    fixBtn =
      button
        [ class "button"
        , if not m.clean then
            onClick addr (fromAction Fix)
          else
            disabled True
        ]
        [ text "Fix" ]

    resetBtn =
      button
        [ class "button"
        , if not m.clean then
            onClick addr (fromAction Reset)
          else
            disabled True
        ]
        [ text "Reset" ]

    undoBtn =
      button
        [ class "button"
        , if m.predecessor /= Nothing then
            onClick addr (fromAction Undo)
          else
            disabled True
        ]
        [ text "Undo" ]

    redoBtn =
      button
        [ class "button"
        , if m.successor /= Nothing then
            onClick addr (fromAction Redo)
          else
            disabled True
        ]
        [ text "Redo" ]

    showCandidatesBtn =
      button
        [ class "button huge"
        , onClick addr (fromAction (ShowCandidates (not m.showCandidates)))
        ]
        [ text (if' m.showCandidates "Hide Candidates" "Edit Candidates") ]

    solutionBtn =
      button
        [ class "button big danger"
        , if not complete then
            onClick addr (fromNotification RequestSolution)
          else
            disabled True
        ]
        [ text "Solution" ]

    status =
      p [ id "status" ] [ text msg ]

    msg =
      if complete && String.isEmpty m.statusMessage then
        if' m.cheated "" "Congratulations."
      else
        m.statusMessage

    mainClass =
      [ class (if' (not m.cheated && complete) "solved" "") ]

    complete =
      allFilledIn m.sudoku
  in
    div (id "main" :: mainClass) [ toolbox, board, status ]


viewCell : Address News -> ( Pos, SC.Model ) -> Html
viewCell addr ( p, scm ) =
  let
    childAddr =
      forwardToChild addr (UpdateCell p) (cellNotificationHandler p)
  in
    td [ class "cell" ] [ div [] [ SC.view childAddr scm ] ]


update : Action -> Model -> Model
update act m =
  case act of
    Load sudoku ->
      let
        m' =
          (makeHistory m << initPremodel)
            { sudoku = sudoku
            , fixInitialNonEmptyEntries = True
            , showCandidates = m.showCandidates
            }
      in
        { m' | isProgenitor = True, clean = True, cheated = False }

    LoadSolution solution ->
      let
        load sudoku =
          (makeHistory (markCheated m)) (initWithSolution m sudoku)
      in
        case solution of
          NoSolution ->
            updateStatusMessage "No solution." m

          MultipleSolutions sudoku ->
            updateStatusMessage "Multiple solutions." (load sudoku)

          Solution sudoku ->
            load sudoku

    Clear ->
      update (Load (blankSudoku m.sudoku.n)) m

    Fix ->
      let
        f _ c sc =
          if isFilledIn c then
            SC.update (SC.ECAction (EC.SetFixed True)) sc
          else
            sc
      in
        (markClean << makeHistory m << toPremodel) (updateCellsZipped m f)

    Reset ->
      Maybe.withDefault m (wipeSlateClean m)

    Undo ->
      Maybe.withDefault m (toPast m)

    Redo ->
      Maybe.withDefault m (toFuture m)

    ShowCandidates enable ->
      let
        msg =
          if enable then
            (join "\n")
              [ "To remove a candidate, click on it."
              , "Clicking on the last candidate in a cell selects it."
              ]
          else
            ""
      in
        (updateStatusMessage msg << reinitCells) { m | showCandidates = enable }

    HandleEntryClick p ->
      let
        m' =
          mapWithDefault m f m.selected

        ( m'', msg ) =
          if pFixed then
            ( m', "" )
          else if pEmpty then
            ( updateCellZipped m' p cInit, "" )
          else
            Maybe.withDefault ( m', warning ) (m.selected `Maybe.andThen` g)

        f ( q, x ) =
          if' (q /= p) (updateCellZipped m q eInit) m

        g ( q, x ) =
          if' (q == p) (Just ( update (SetUnknown p) m', "" )) Nothing

        ( pEmpty, pFixed ) =
          case psc of
            Just (SC.ECModel ec) ->
              ( ec.value == Nothing, ec.fixed )

            _ ->
              ( False, False )

        eInit c sc =
          case ( m.showCandidates, sc ) of
            ( False, SC.CCModel _ ) ->
              SC.initWithNonFixedEntry c

            _ ->
              sc

        cInit c _ =
          SC.tryToInitWithCandidates c m.sudoku.n True

        warning =
          "To remove, click again."

        psc =
          Array.get p m.cells
      in
        { m''
          | selected = Maybe.map (\x -> ( p, x )) psc
          , statusMessage = msg
        }

    HandleCandidateClick v p ->
      update ((if' m.showCandidates ToggleCandidate SetValue) v p) m

    SetUnknown p ->
      let
        pm' =
          simpleElimination (mapWithDefault pm f mv)

        pm =
          { sudoku = setCandidates ac p m.sudoku
          , cells = Array.set p sc m.cells
          }

        f v =
          clustersXFoldl p (candidateUpdate (insert v)) pm

        sc =
          SC.tryToInitWithCandidates (Candidates ac) m.sudoku.n m.showCandidates

        ac =
          allCandidates m.sudoku.n

        mv =
          Array.get p m.sudoku.board `Maybe.andThen` filledIn
      in
        makeHistory m { sudoku = pm'.sudoku, cells = pm'.cells }

    SetValue v p ->
      let
        f candidates =
          if member v candidates then
            (makeHistory m << clustersXFoldl p (candidateUpdate (remove v)))
              { sudoku = setValue v p m.sudoku
              , cells = Array.set p sc m.cells
              }
          else
            m

        sc =
          (SC.ECModel << EC.init) { value = Just v, fix = False }

        maybeCandidates p =
          Array.get p m.sudoku.board `Maybe.andThen` candidates
      in
        mapWithDefault m f (maybeCandidates p)

    ToggleCandidate v p ->
      let
        f candidates =
          if isEmpty (toggle v candidates) then
            update (SetValue v p) m
          else
            (makeHistory m)
              { sudoku = toggleCandidate v p m.sudoku
              , cells = arrayUpdate p g m.cells
              }

        g sc =
          SC.update (SC.CCAction (CC.ToggleEntry v)) sc

        maybeCandidates p =
          Array.get p m.sudoku.board `Maybe.andThen` candidates
      in
        if occursInClustersX m.sudoku p v then
          m
        else
          mapWithDefault m f (maybeCandidates p)

    UpdateCell p scAct ->
      updateCell m p (SC.update scAct)


updateCell : Model -> Pos -> (SC.Model -> SC.Model) -> Model
updateCell m p f =
  { m | cells = arrayUpdate p f m.cells }


updateCells : Model -> (Pos -> SC.Model -> SC.Model) -> Model
updateCells m f =
  { m | cells = Array.indexedMap f m.cells }


updateCellZipped : Model -> Pos -> (Cell -> SC.Model -> SC.Model) -> Model
updateCellZipped m p f =
  { m | cells = arrayUpdateZipped p f m.sudoku.board m.cells }


updateCellsZipped : Model -> (Pos -> Cell -> SC.Model -> SC.Model) -> Model
updateCellsZipped m f =
  { m | cells = arrayZippedIndexedMap f m.sudoku.board m.cells }


updateStatusMessage : String -> Model -> Model
updateStatusMessage msg m =
  { m | statusMessage = msg }


markClean : Model -> Model
markClean m =
  { m | clean = True }


markCheated : Model -> Model
markCheated m =
  { m | cheated = True }


makeHistory : Model -> Premodel -> Model
makeHistory present future =
  -- NOTE: Doesn't work as expected.
  -- Comparing Sets (and Arrays?) for equality is broken in Elm 0.16.0.
  if future /= toPremodel present then
    let
      m =
        { present | cheated = present.cheated || allFilledIn present.sudoku }
    in
      tidyUpdate
        { m
          | sudoku = future.sudoku
          , cells = future.cells
          , predecessor = Just (toHistoricModel m)
          , successor = Nothing
          , isProgenitor = False
          , clean = False
        }
  else
    present


toFuture : Model -> Maybe Model
toFuture present =
  let
    f (HistoricModel future) =
      { present
        | sudoku = future.sudoku
        , cells = future.cells
        , predecessor = Just (toHistoricModel present)
        , successor = future.successor
        , isProgenitor = future.isProgenitor
        , clean = future.clean
        , cheated =
            if future.isProgenitor then
              future.cheated
            else
              present.cheated || future.cheated
      }
  in
    Maybe.map (tidyUpdate << f) present.successor


toPast : Model -> Maybe Model
toPast present =
  let
    f (HistoricModel past) =
      { present
        | sudoku = past.sudoku
        , cells = past.cells
        , predecessor = past.predecessor
        , successor = Just (toHistoricModel present)
        , isProgenitor = past.isProgenitor
        , clean = past.clean
        , cheated =
            if present.isProgenitor then
              past.cheated
            else
              present.cheated || allFilledIn present.sudoku
      }
  in
    Maybe.map (tidyUpdate << f) present.predecessor


wipeSlateClean : Model -> Maybe Model
wipeSlateClean present =
  let
    f (HistoricModel hm) =
      if hm.clean then
        Just
          { present
            | sudoku = hm.sudoku
            , cells = hm.cells
            , predecessor = Just (toHistoricModel present)
            , successor = Nothing
            , clean = True
            , cheated = present.cheated || allFilledIn present.sudoku
          }
      else
        hm.predecessor `Maybe.andThen` f

    mm' =
      Maybe.map tidyUpdate (present.predecessor `Maybe.andThen` f)
  in
    if' present.clean (Just present) mm'


tidyUp : Model -> Model
tidyUp m =
  { m | selected = Nothing, statusMessage = "" }


tidyUpdate : Model -> Model
tidyUpdate m =
  (reinitCells << tidyUp) m


reinitCells : Model -> Model
reinitCells m =
  let
    f _ c sc =
      if' (not (isFilledIn c)) (g c) sc

    g c =
      if m.showCandidates then
        SC.tryToInitWithCandidates c m.sudoku.n m.showCandidates
      else
        SC.initWithNonFixedEntry c
  in
    updateCellsZipped m f


initWithSolution : Model -> Sudoku -> Premodel
initWithSolution m solution =
  let
    f c sc =
      case ( c, sc ) of
        ( FilledIn v, SC.ECModel ecm ) ->
          SC.ECModel { ecm | value = Just v }

        ( FilledIn v, _ ) ->
          (SC.ECModel << EC.init) { value = Just v, fix = False }

        _ ->
          sc
  in
    { sudoku = solution, cells = arrayZippedMap f solution.board m.cells }


clustersXFoldl : Pos -> (Pos -> Cell -> Premodel -> Premodel) -> Premodel -> Premodel
clustersXFoldl p f pm =
  Sudoku.clustersXFoldl pm.sudoku p f pm


candidateUpdate : (Values -> Values) -> Pos -> Cell -> Premodel -> Premodel
candidateUpdate f p c pm =
  let
    g vs =
      let
        vs' =
          f vs
      in
        { pm
          | sudoku = setCandidates vs' p pm.sudoku
          , cells = arrayUpdate p (sc' vs') pm.cells
        }

    sc' vs =
      SC.update (SC.CCAction (CC.SetCandidates vs))
  in
    mapWithDefault pm g (candidates c)


simpleElimination : Premodel -> Premodel
simpleElimination pm =
  let
    f p c pm =
      mapWithDefault pm (rm p pm) (filledIn c)

    rm p pm v =
      clustersXFoldl p (candidateUpdate (remove v)) pm
  in
    arrayIndexedFoldl f pm pm.sudoku.board


cellNotificationHandler : Pos -> SC.Notification -> News
cellNotificationHandler p ntf =
  case ntf of
    SC.ECNotification (EC.Clicked) ->
      fromAction (HandleEntryClick p)

    SC.CCNotification (CC.Clicked v) ->
      fromAction (HandleCandidateClick v p)
