module Array2d exposing
  ( Array2d
  , empty
  , isEmpty
  , width
  , height
  , repeat
  , get
  , set
  , map
  , indexedMap
  , fromList
  , toList
  )

import Array exposing (Array)

type alias Array2d a = Array (Array a)

empty =
  [ Array.empty ] |> Array.fromList

isEmpty arr2 =
  (==) arr2 empty

width arr2 =
  Maybe.withDefault Array.empty
    ( Array.get 0 arr2)
      |> Array.length

height arr2 =
  Array.length arr2

repeat (x,y) a =
  Array.repeat y (Array.repeat x a)

get (x,y) arr2 =
  case Array.get y arr2 of
    Just a -> Array.get x a
    _ -> Nothing

set (x,y) a arr2 =
  Array.set y
    ( ( Maybe.withDefault
        Array.empty
        ( Array.get y arr2 )
      )
      |> Array.set x a
    )
    arr2

map f arr2 =
  Array.map
    (Array.map f)
    arr2

indexedMap f arr2 =
  Array.indexedMap
    (\y arr ->
      Array.indexedMap
        (\x a -> f (x,y) a )
        arr
    ) arr2

fromList li2 =
  List.map Array.fromList li2
    |> Array.fromList

toList arr2 =
  Array.map Array.toList arr2
    |> Array.toList