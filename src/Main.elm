module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import List exposing (range)
import Html exposing (Html, button, div, h1, h3, text, h4)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


main =
  Browser.document { init = init, update = update, view = view , subscriptions = \_ -> Sub.none}

--Model

type alias Document msg =
  { title : String
  , body : List (Html msg)
  }

type Player 
  = Black 
  | White

type alias GameBoard 
  = Array Square

type Square
  = Occupied Unit
  | Unoccupied

type Unit
  = Pawn Player
  | Rook Player
  | Knight Player
  | Bishop Player
  | Queen Player
  | King Player

type alias Model =
  { board : GameBoard
  , currentPlayer : Player
  , activePiece : Maybe Unit
  }

type Msg
  = Reset


init : () -> ( Model, Cmd Msg )
init _ = 
  ( { board = defaultBoard
    , currentPlayer = White
    , activePiece = Nothing }, Cmd.none)


defaultBoard: Array Square
defaultBoard = 
  Array.fromList [ Occupied (Rook Black)
  , Occupied (Knight Black)
  , Occupied (Bishop Black)
  , Occupied (King Black)
  , Occupied (Queen Black)
  , Occupied (Bishop Black)
  , Occupied (Knight Black)
  , Occupied (Rook Black)
  , Occupied (Pawn Black)
  , Occupied (Pawn Black)
  , Occupied (Pawn Black)
  , Occupied (Pawn Black)
  , Occupied (Pawn Black)
  , Occupied (Pawn Black)
  , Occupied (Pawn Black)
  , Occupied (Pawn Black)
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Unoccupied
  , Occupied (Pawn White)
  , Occupied (Pawn White)
  , Occupied (Pawn White)
  , Occupied (Pawn White)
  , Occupied (Pawn White)
  , Occupied (Pawn White)
  , Occupied (Pawn White)
  , Occupied (Pawn White)
  , Occupied (Rook White)
  , Occupied (Knight White)
  , Occupied (Bishop White)
  , Occupied (Queen White)
  , Occupied (King  White)
  , Occupied (Bishop White)
  , Occupied (Knight White)
  , Occupied (Rook White) ]


changePlayer : Player -> Player
changePlayer player =
  case player of
    Black -> White
    White -> Black

--Update

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      ( 
        { board = defaultBoard
        , currentPlayer = White
        , activePiece = Nothing
      }, Cmd.none)

--View

view : Model -> Document Msg
view model =
  { title = "Elm-Chess"
  , body = 
    [ h1 [] [ text "Elm Chess"] 
      , div [class "game-content"] (range 1 8 |> List.map (viewBoardRow model.board)) 
    ] }


viewBoardRow : GameBoard -> Int -> Html Msg 
viewBoardRow board index =
  let 
    rangeStart = (index - 1) * 8 
    rangeEnd = index * 8 - 1 
  in  
    div [ class "row" ] (range rangeStart rangeEnd |> List.map (viewBoardButton board))


viewBoardButton : GameBoard -> Int -> Html Msg 
viewBoardButton board index =
  div [ class ("square square-" ++ (String.fromInt index)) ] [ Array.get index board |> viewBoardButtonText |> text ]

viewBoardButtonText : Maybe Square -> String
viewBoardButtonText maybeSquare =
  case maybeSquare of
    Just squareValue ->
      case squareValue of
        Occupied unit ->
          case unit of
            Pawn player ->
              case player of
                Black ->
                  "B-P"
                White ->
                  "W-P"
            Rook player -> 
              case player of
                Black ->
                  "B-R"
                White ->
                  "W-R"
            Knight player ->
              case player of
                Black ->
                  "B-K"
                White ->
                  "W-K"
            Bishop player ->
              case player of
                Black ->
                  "B-B"
                White ->
                  "W-B"
            Queen player ->
              case player of
                Black ->
                  "B-Q"
                White ->
                  "W-Q"
            King player ->
              case player of
                Black ->
                  "B-King"
                White ->
                  "W-King"
        Unoccupied ->
          "E"
    Nothing ->
      "E"
