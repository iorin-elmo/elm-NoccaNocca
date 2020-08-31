module Main exposing (..)

import Browser
import Html exposing (Html, div, br, button)
import Html.Events
import Html.Attributes
import Svg exposing (Svg, svg )
import Svg.Events as SvgEv
import Svg.Attributes as SvgAt

import Array2d as Arr2 exposing (Array2d)
import Debug

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

type Piece
  = Black
  | White

type alias Model =
  { piecePosition : Array2d (List Piece)
  , movable : Array2d Bool
  , turn : Piece
  , selectedPiece : Maybe ( Int, Int )
  }

init =
  { piecePosition =
    [ List.repeat 5 [White] ] ++
    List.repeat 4 (List.repeat 5 []) ++
    [ List.repeat 5 [Black] ]
      |> Arr2.fromList
  , movable =
    Arr2.repeat (5,6) False
  , turn = Black
  , selectedPiece = Nothing
  }

type Msg
  = SelectPos (Int,Int)
  | MovePos (Int,Int)
  | BackToSelect

update : Msg -> Model -> Model
update msg model =
  case msg of
    SelectPos (x,y) ->
      let
        movableCheck pos_ =
          Arr2.get pos_
            model.piecePosition
            |> Maybe.map
              (\li -> List.length li < 3)
            |> Maybe.withDefault True
        checkList =
          [ (x-1,y-1),(x,y-1),(x+1,y-1)
          , (x-1,y  ),        (x+1,y  )
          , (x-1,y+1),(x,y+1),(x+1,y+1)]
      in
        { model
        | selectedPiece = Just (x,y)
        , movable =
            checkList
              |> List.foldl
                (\pos_ field ->
                  Arr2.set pos_
                  (movableCheck pos_)
                  field
                )
                (Arr2.repeat (5,6) False)
        }
    MovePos pos ->
      { selectedPiece = Nothing
      , movable = Arr2.repeat (5,6) False
      , turn = if model.turn == Black then White else Black
      , piecePosition =
        let
          selectedPos =
            model.selectedPiece
              |> Maybe.withDefault (-1,0)
          rmArrFromSelectedPiece =
            let
              li =
                Arr2.get selectedPos
                  model.piecePosition
                  |> Maybe.withDefault []
            in
              case li of
                [] -> model.piecePosition
                hd::tl ->
                  Arr2.set selectedPos tl
                    model.piecePosition
        in
          Arr2.get pos
            rmArrFromSelectedPiece
            |> Maybe.withDefault []
            |> (::) model.turn
            |> (\a ->
                Arr2.set pos a
                  rmArrFromSelectedPiece )
      }
    BackToSelect ->
      { model
      | movable = Arr2.repeat (5,6) False
      , selectedPiece = Nothing
      }

view : Model -> Html Msg
view model =
  let


    -- For Drawing Pieces
    makeCircle (x,y) n col bool =
      Svg.circle
        [ x*50+25
            |> (SvgAt.cx << String.fromInt)
        , y*50+25
            |> (SvgAt.cy << String.fromInt)
        , 3*n+15
            |> (SvgAt.r  << String.fromInt)
        , ( if col == Black
          then "black" else "white" )
            |> SvgAt.fill
        , --( if col == Black
          --then "white" else "black" )
          "gray"  |> SvgAt.stroke
        , SvgAt.strokeWidth "1"
        , if bool
          then SvgEv.onClick <| SelectPos (x,y)
          else SvgAt.strokeWidth "1"
        ][]


    pieceView =
      model.piecePosition
        |> Arr2.indexedMap
          (\(x,y) li ->
            li
              |> List.indexedMap
                (\n col ->
                  makeCircle (x,y)
                    (3 - List.length li + n)
                    col
                    ((n==0) && (col==model.turn))
                )
              |> List.reverse
          )
        |> Arr2.toList
        |> List.concat
        |> List.concat


    --For Drawing Fields
    makeSquare (x,y) bool =
      Svg.rect
        [ ( SvgAt.x << String.fromInt ) (x*50)
        , ( SvgAt.y << String.fromInt ) (y*50)
        , SvgAt.width "50"
        , SvgAt.height "50"
        , SvgAt.fill "red"
        , SvgAt.fillOpacity (if bool then "0.2" else "0")
        , SvgAt.stroke "Black"
        , SvgAt.strokeWidth "1"
        , if bool
          then SvgEv.onClick <| MovePos (x,y)
          else SvgAt.strokeWidth "1"
        ][]

    fieldView =
      model.movable
        |> Arr2.indexedMap makeSquare
        |> Arr2.toList
        |> List.concat

    forBackToSelect =
      case model.selectedPiece of
        Just (x,y) ->
          case Arr2.get (x,y) model.piecePosition of
            Just li ->
              [ Svg.circle
                  [ x*50+25
                      |> (SvgAt.cx << String.fromInt)
                  , y*50+25
                      |> (SvgAt.cy << String.fromInt)
                  , (3 - List.length li)*3+15
                      |> (SvgAt.r  << String.fromInt)
                  , ( if model.turn == Black
                    then "black" else "white" )
                      |> SvgAt.fill
                  , --( if col == Black
                    --then "white" else "black" )
                    "gray"  |> SvgAt.stroke
                  , SvgAt.strokeWidth "1"
                  , SvgEv.onClick BackToSelect
                  ][]
              ]
            _ -> []
        _ -> []

  in
    svg
      [ SvgAt.width "251"
      , SvgAt.height "301"
      , SvgAt.viewBox "0 0 251 301"
      ]
      ( if model.selectedPiece == Nothing
        then
          (fieldView++pieceView)
        else
          (pieceView++fieldView++forBackToSelect)
      )

