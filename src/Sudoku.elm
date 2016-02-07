module Sudoku (..) where

import Array exposing (Array)
import Utilities exposing (..)
import Set exposing (Set)


type alias Sudoku =
  { board : Board, clusters : Clusters, n : Int, n2 : Int, n4 : Int }


type alias Board =
  Array Cell


type Cell
  = FilledIn Value
  | Candidates Values


type alias Clusters =
  List Cluster


type alias Cluster =
  Set Pos


type SolutionResponse
  = NoSolution
  | Solution Sudoku
  | MultipleSolutions Sudoku


allFilledIn : Sudoku -> Bool
allFilledIn sudoku =
  arrayAll isFilledIn sudoku.board


blankSudoku : Int -> Sudoku
blankSudoku n =
  let
    acs =
      Candidates (allCandidates n)
  in
    init n <| Array.initialize (n * n * n * n) (always acs)


init : Int -> Board -> Sudoku
init order board =
  let
    rows r c =
      r * n2 + c

    cols c r =
      r * n2 + c

    boxs b p =
      ((b // n) * n2 + b % n) * n + (p // n) * n2 + p % n

    make cluster =
      List.map (\i -> Set.map (cluster i) n2Set) n2List

    n2Set =
      Set.fromList n2List

    n2List =
      [0..(n2 - 1)]

    ( n, n2, n4 ) =
      f order

    f x =
      let
        ( x2, x4 ) =
          ( x * x, x * x * x * x )
      in
        if x4 <= Array.length board then
          ( x, x2, x4 )
        else
          f (x - 1)
  in
    { n = n
    , n2 = n2
    , n4 = n4
    , board = board
    , clusters = List.concat [ make rows, make cols, make boxs ]
    }


setValue : Value -> Pos -> Sudoku -> Sudoku
setValue v p sudoku =
  { sudoku | board = Array.set p (FilledIn v) sudoku.board }


setCandidates : Values -> Pos -> Sudoku -> Sudoku
setCandidates candidates p sudoku =
  { sudoku | board = Array.set p (Candidates candidates) sudoku.board }


toggleCandidate : Value -> Pos -> Sudoku -> Sudoku
toggleCandidate v p sudoku =
  let
    f candidates =
      toggle v candidates
  in
    { sudoku | board = arrayUpdate p (mapToCandidates f) sudoku.board }


clustersFoldl : Sudoku -> (Pos -> Cell -> a -> a) -> a -> a
clustersFoldl sudoku f a =
  List.foldl (clusterFoldl sudoku f) a sudoku.clusters


clustersXFoldl : Sudoku -> Pos -> (Pos -> Cell -> a -> a) -> a -> a
clustersXFoldl sudoku p f a =
  let
    clusters =
      List.filter (Set.member p) sudoku.clusters
  in
    List.foldl (clusterFoldl sudoku f) a clusters


clusterFoldl : Sudoku -> (Pos -> Cell -> a -> a) -> Cluster -> a -> a
clusterFoldl sudoku f cluster a =
  Set.foldl (applyToCell sudoku f) a cluster


applyToCell : Sudoku -> (Pos -> Cell -> a -> a) -> Pos -> a -> a
applyToCell sudoku f p a =
  mapWithDefault a (\c -> f p c a) (Array.get p sudoku.board)


occursInClustersX : Sudoku -> Pos -> Value -> Bool
occursInClustersX sudoku p v =
  let
    f _ c a =
      mapWithDefault a (\w -> a || w == v) (filledIn c)
  in
    clustersXFoldl sudoku p f False


type alias SerializedSudoku =
  { n : Int, board : List (List Int) }


serialize : Sudoku -> SerializedSudoku
serialize s =
  let
    f x =
      case x of
        FilledIn v ->
          [ v ]

        Candidates candidates ->
          toList candidates
  in
    { n = s.n, board = List.map f (Array.toList s.board) }


deserialize : SerializedSudoku -> Sudoku
deserialize s =
  let
    f x =
      case x of
        v :: [] ->
          FilledIn v

        _ ->
          Candidates (fromList x)
  in
    init s.n <| Array.map f (Array.fromList s.board)



-- Value


allCandidates : Int -> Values
allCandidates n =
  fromList [0..(n * n - 1)]


type alias Values =
  Set Value


type alias Value =
  Int


fromList : List Value -> Values
fromList =
  Set.fromList


toList : Values -> List Value
toList =
  Set.toList


isEmpty : Values -> Bool
isEmpty =
  Set.isEmpty


size : Values -> Int
size =
  Set.size


member : Value -> Values -> Bool
member =
  Set.member


insert : Value -> Values -> Values
insert =
  Set.insert


remove : Value -> Values -> Values
remove =
  Set.remove


toggle : Value -> Values -> Values
toggle v vs =
  if' (member v vs) (remove v vs) (insert v vs)



-- Convenience functions


isFilledIn : Cell -> Bool
isFilledIn x =
  case x of
    FilledIn _ ->
      True

    _ ->
      False


filledIn : Cell -> Maybe Value
filledIn x =
  case x of
    FilledIn a ->
      Just a

    _ ->
      Nothing


candidates : Cell -> Maybe Values
candidates x =
  case x of
    Candidates b ->
      Just b

    _ ->
      Nothing


mapToCandidates : (Values -> Values) -> Cell -> Cell
mapToCandidates f x =
  case x of
    Candidates b ->
      Candidates (f b)

    _ ->
      x
