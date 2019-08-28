module Main exposing (Model, Msg(..), init, main, update, view)

import Array
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

type alias Model =
  {}

type Player 
  = PlayerB 
  | PlayerW

type Msg
  = Reset



init : () -> ( Model, Cmd Msg )
init _ = 
  ( {}, Cmd.none)


changePlayer : Player -> Player
changePlayer player =
  case player of
    PlayerB -> PlayerW
    PlayerW -> PlayerB

--Update

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      ( model, Cmd.none)

--View

view : Model -> Document Msg
view model =
  { title = "Elm-Chess"
  , body = [ h1 [] [ text "Elm Chess"] ] }
