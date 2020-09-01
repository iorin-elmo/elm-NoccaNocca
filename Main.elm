module Main exposing (..)

import Browser
import Html exposing (Html, div, br, button, text, option, select)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value)
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
  , size : ( Int, Int )
  , phase : Int
  }

init =
  { piecePosition = Arr2.empty
  , movable = Arr2.empty
  , turn = White
  , selectedPiece = Nothing
  , size = (2,2)
  , phase = 0
  }

type Msg
  = SelectPos (Int,Int)
  | MovePos (Int,Int)
  | BackToSelect
  | Start
  | InputWidth String
  | InputHeight String
  | AIMove

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
                (Arr2.repeat model.size False)
        }
    MovePos pos ->
      { model
      | piecePosition =
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
        |> turnEndUpdate

    BackToSelect ->
      { model
      | movable = Arr2.repeat model.size False
      , selectedPiece = Nothing
      }
    Start ->
      { model
      | movable = Arr2.repeat model.size False
      , piecePosition =
        (\(w,h) ->
          [ List.repeat w [White] ] ++
          List.repeat (h-2) (List.repeat w []) ++
          [ List.repeat w [Black] ]
            |> Arr2.fromList
        )
        model.size
      , phase = 1
      }
    InputWidth w ->
      { model
      | size =
        model.size
          |> (\(_,b) ->
              ( Maybe.withDefault 5 (String.toInt w)
              ,b )
              )
      }
    InputHeight h ->
      { model
      | size =
        model.size
          |> (\(a,_) ->
              ( a
              , Maybe.withDefault 6 (String.toInt h)
              ) )
      }
    AIMove ->
      { model
      | piecePosition =
        aiSearch 5 model.turn (Tuple.second model.size) model.piecePosition
         |> Tuple.second
      }
        |> turnEndUpdate

aiSearch n col ysize field =
  if n == 0
  then (evaluate ysize field, field)
  else
    findAll col field
      |> List.map
        (\f -> aiSearch (n-1)
          ( if col == Black then White else Black ) ysize f
          |> (\(val,_) -> (val,f))
        )
      |> List.foldl
        (\(v,f) (rsv,res) ->
          if modBy 2 n == 1
          then
            if v < rsv then (rsv,res) else (v,f)
          else
            if v > rsv then (rsv,res) else (v,f)
        )
        (if modBy 2 n == 1 then (evalMin,field) else (evalMax,field))


evalMax =  10000
evalMin = -10000

evaluate ysize field =
  Arr2.indexedMap
    (\(_,y) li ->
      case li of
        hd::tl ->
          ( if hd == Black
            then (ysize-y)*2
            else -y*2
          )
        _ -> 0
    ) field
    |> Arr2.toList
    |> List.concat
    |> List.sum

findAll col field =
  Arr2.indexedMap
    (\(x,y) li ->
      case li of
        hd::tl ->
          if hd == col
          then
            let
              nf = Arr2.set (x,y) tl field
            in
              [(x-1,y-1),(x,y-1),(x+1,y-1)
              ,(x-1,y  ),        (x+1,y  )
              ,(x-1,y+1),(x,y+1),(x+1,y+1)]
                |> List.concatMap
                  (\(nx,ny) ->
                    case Arr2.get (nx,ny) nf of
                      Just mli ->
                        if List.length mli /= 3
                        then
                          Arr2.set (nx,ny) (col::mli) nf
                            |> List.singleton
                        else []
                      _ -> []
                  )
          else []
        _ -> []
    ) field
    |> Arr2.toList
    |> List.concat
    |> List.concat

turnEndUpdate model = -- 2:white win 3:black win
  let
    isBlackMovable =
      Arr2.fold
        (\li res -> (List.head li == Just Black) || res)
        False
        model.piecePosition
    isWhiteMovable =
      Arr2.fold
        (\li res -> (List.head li == Just White) || res)
        False
        model.piecePosition
    isBlackReached =
      Arr2.indexedMap
        (\(x,y) li ->
          if y == 0
          then
            if (List.length li == 3)&&(List.head li == Just Black)
            then
              True
            else
              List.head li == Just Black
          else False
        ) model.piecePosition
        |> Arr2.fold (||) False
    isWhiteReached =
      Arr2.indexedMap
        (\(x,y) li ->
          if y == Tuple.second model.size - 1
          then
            if (List.length li == 3)&&(List.head li == Just White)
            then
              True
            else
              List.head li == Just White
          else False
        ) model.piecePosition
        |> Arr2.fold (||) False
    newPhase =
      if model.turn == White
      then
        if isBlackMovable
        then
          if isBlackReached
          then 3
          else
            1
        else 2
      else
        if isWhiteMovable
        then
          if isWhiteReached
          then 2 else 1
        else 3
  in
    { model
    | phase = newPhase
    , turn = if model.turn == Black then White else Black
    , selectedPiece = Nothing
    , movable = Arr2.repeat model.size False
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
    let
      length2Str n = n*50+1 |> String.fromInt
      (w,h) =
        model.size
          |> Tuple.mapBoth length2Str length2Str
    in
      case model.phase of
        0 ->
          let
            selectOptions =
              List.range 2 10
                |> List.map
                  (\n ->
                    option
                      [ value
                        ( String.fromInt n) ]
                      [ text
                        ( String.fromInt n) ]
                  )
          in
            div []
            [ text "width : "
            , select
              [ onInput InputWidth ]
              selectOptions
            , br [][]
            , text "height : "
            , select
              [ onInput InputHeight ]
              selectOptions
            , br [][]
            , button
              [ onClick Start ]
              [ text "Start"]
            ]
        n ->
          div []
          [ svg
            [ SvgAt.width w
            , SvgAt.height h
            , SvgAt.viewBox ("0 0 "++w++" "++h)
            ]
            ( if model.selectedPiece == Nothing
              then
                (fieldView++pieceView)
              else
                (pieceView++fieldView++forBackToSelect)
            )
          , br [][]
          , text
            ( case n of
              2 -> "White Wins!"
              3 -> "Black Wins!"
              _ -> ""
            )
          , br [][]
          , button [ onClick AIMove ][ text "AIMove" ]
          ]

