module Utilities (..) where

import Array exposing (Array)


-- General


if' : Bool -> a -> a -> a
if' x y z =
  if x then
    y
  else
    z



-- Maybe


mapWithDefault : b -> (a -> b) -> Maybe a -> b
mapWithDefault b f =
  Maybe.withDefault b << Maybe.map f



-- List


listGroupBy : Int -> List a -> List (List a)
listGroupBy n x =
  if List.isEmpty x then
    []
  else
    (List.take n x) :: listGroupBy n (List.drop n x)



-- Array


type alias Pos =
  Int


arrayAll : (a -> Bool) -> Array a -> Bool
arrayAll f =
  Array.foldl (\a b -> b && f a) True


arrayAny : (a -> Bool) -> Array a -> Bool
arrayAny f =
  Array.foldl (\a b -> b || f a) False


arrayIndexedFoldl : (Pos -> a -> b -> b) -> b -> Array a -> b
arrayIndexedFoldl f b =
  snd << Array.foldl (\a ( i, b' ) -> ( i + 1, f i a b' )) ( 0, b )


arrayUpdate : Pos -> (a -> a) -> Array a -> Array a
arrayUpdate i f x =
  let
    g a =
      Array.set i (f a) x
  in
    mapWithDefault x g (Array.get i x)


arrayUpdateZipped : Pos -> (a -> b -> b) -> Array a -> Array b -> Array b
arrayUpdateZipped i f x y =
  case ( Array.get i x, Array.get i y ) of
    ( Just a, Just b ) ->
      Array.set i (f a b) y

    _ ->
      y


arrayZippedIndexedMap : (Pos -> a -> b -> c) -> Array a -> Array b -> Array c
arrayZippedIndexedMap f x y =
  let
    g i a =
      (f i) a (arrayUnsafeGet i y)

    h i b =
      (f i) (arrayUnsafeGet i x) b
  in
    if Array.length x <= Array.length y then
      Array.indexedMap g x
    else
      Array.indexedMap h y


arrayZippedMap : (a -> b -> c) -> Array a -> Array b -> Array c
arrayZippedMap f =
  arrayZippedIndexedMap (always f)


arrayUnsafeGet : Pos -> Array a -> a
arrayUnsafeGet i x =
  case Array.get i x of
    Just v ->
      v

    Nothing ->
      Debug.crash "out of range"
