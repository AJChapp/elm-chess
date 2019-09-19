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
  = Array (Array Square)

type Square
  = Occupied Unit
  | Unoccupied
  | ErrorSquare

type Unit
  = Pawn Player
  | Rook Player
  | Knight Player
  | Bishop Player
  | Queen Player
  | King Player
  | ErrorUnit

type alias Model =
  { board : GameBoard
  , currentPlayer : Player
  , activePiece : 
    { unit: Maybe Unit 
    , unitPosition: Maybe (Int, Int) }
  }

type Msg
  = Reset
  | Error
  | SelectPiece Unit (Int, Int)
  | DeselectPiece
  | MoveUnit (Int, Int) (Int,Int) Unit
  | DoNothing

init : () -> ( Model, Cmd Msg )
init _ = 
  ( { board = defaultBoard
    , currentPlayer = White
    , activePiece = 
      { unit = Nothing
      , unitPosition = Nothing }
  }, Cmd.none)


generatePawnMoveList : GameBoard -> Player -> (Int, Int) -> Array (Int, Int)
generatePawnMoveList board owner (rowIndex, columnIndex) =
  case owner of
    Black ->
      let
          --TODO rename moveLists
        baseMoveList = Array.fromList [ (1, 0) ]
        moveList_v2 = 
          if rowIndex == 1 then
            --TODO implement En Passant
            Array.fromList [ (2, 0) ] |> Array.append baseMoveList 
          else
            baseMoveList
        moveList_v3 = 
          if isPositionValid (rowIndex + 1, columnIndex + 1) then 
            case getSquare board (rowIndex + 1, columnIndex + 1) of
              Occupied unit ->
                Array.fromList [ (1, 1) ] |> Array.append moveList_v2
              _ ->
                moveList_v2
          else
            moveList_v2
        moveList_v4 = 
          if isPositionValid (rowIndex + 1, columnIndex - 1) then
            case getSquare board (rowIndex + 1, columnIndex + 1) of
              Occupied unit ->
                Array.fromList [ (1, -1) ] |> Array.append moveList_v3
              _ ->
                moveList_v3
          else 
            moveList_v3
      in 
        moveList_v4
    White ->
      let
          --TODO rename moveLists
        baseMoveList = Array.fromList [ (-1, 0) ]
        moveList_v2 = 
          if rowIndex == 6 then
            --TODO implement En Passant
            Array.fromList [ (-2, 0) ] |> Array.append baseMoveList
          else
            baseMoveList
        moveList_v3 = 
          if isPositionValid (rowIndex - 1, columnIndex + 1) then 
            case getSquare board (rowIndex - 1, columnIndex + 1) of
              Occupied unit ->
                Array.fromList [ (-1, 1) ] |> Array.append moveList_v2 
              _ ->
                moveList_v2
          else
            moveList_v2
        moveList_v4 = 
          if isPositionValid (rowIndex - 1, columnIndex - 1) then 
            case getSquare board (rowIndex - 1, columnIndex - 1) of
              Occupied unit ->
                Array.fromList [ (-1, -1) ] |> Array.append moveList_v3
              _ ->
                moveList_v3
          else
            moveList_v3
      in
        moveList_v4

getUnitMovementOptions : GameBoard -> Unit -> (Int, Int) -> Array (Int, Int)
getUnitMovementOptions board unit unitPosition =
  case unit of
    Pawn owner ->
      case owner of
        Black ->
          generatePawnMoveList board owner unitPosition 
        White ->
          generatePawnMoveList board owner unitPosition 
    Rook owner -> 
      Array.fromList [ (1,2) ]
    Knight owner ->
      Array.fromList [ (2,-1), (2,1), (-1,2), (1,2), (-2,1), (-2,-1), (1,-2), (-1,-2) ]
    Bishop owner ->
      Array.fromList [ (1,2) ]
    Queen owner ->
      Array.fromList [ (1,2) ]
    King owner ->
      Array.fromList [ (1,2) ]
    ErrorUnit -> 
      Array.fromList [ (1,1) ]


isPositionValid : (Int, Int) -> Bool
isPositionValid (yIndex, xIndex) =
  if yIndex < 0 || yIndex > 7 || xIndex < 0 || xIndex > 7 then
    False 
  else 
    True


getPossibleLzs : Array (Int, Int) -> (Int, Int) -> Array (Int, Int)
getPossibleLzs possibleMoves currentPosition =
  let 
    (rowIndex, columnIndex) = currentPosition
    maybePossibleLZ = Array.map (\(y, x) -> (y + rowIndex, x + columnIndex)) possibleMoves
  in  
    Array.filter isPositionValid maybePossibleLZ


getActivePiecePostion : Model -> (Int, Int) 
getActivePiecePostion model = 
  case model.activePiece.unitPosition of
    Just postion ->
      postion
    Nothing ->
      (-9,-9)

defaultBoard: Array (Array Square)
defaultBoard = 
  Array.fromList [ 
  Array.fromList [ Occupied (Rook Black)
    , Occupied (Knight Black)
    , Occupied (Bishop Black)
    , Occupied (King Black)
    , Occupied (Queen Black)
    , Occupied (Bishop Black)
    , Occupied (Knight Black)
    , Occupied (Rook Black) ]
  , Array.fromList [ Occupied (Pawn Black)
    , Occupied (Pawn Black)
    , Occupied (Pawn Black)
    , Occupied (Pawn Black)
    , Occupied (Pawn Black)
    , Occupied (Pawn Black)
    , Occupied (Pawn Black)
    , Occupied (Pawn Black) ]
  , Array.fromList [ Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied ]
  , Array.fromList [ Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied ]
  , Array.fromList [ Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied ]
  , Array.fromList [ Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied
    , Unoccupied ]
  , Array.fromList [ Occupied (Pawn White)
    , Occupied (Pawn White)
    , Occupied (Pawn White)
    , Occupied (Pawn White)
    , Occupied (Pawn White)
    , Occupied (Pawn White)
    , Occupied (Pawn White)
    , Occupied (Pawn White) ]
  , Array.fromList [ Occupied (Rook White)
    , Occupied (Knight White)
    , Occupied (Bishop White)
    , Occupied (Queen White)
    , Occupied (King  White)
    , Occupied (Bishop White)
    , Occupied (Knight White)
    , Occupied (Rook White) ] ]


isEven : Int -> Bool
isEven num = 
  if modBy 2 num == 0 then
    True
  else 
    False


changePlayer : Player -> Player
changePlayer player =
  case player of
    Black -> White
    White -> Black


getSquare : GameBoard -> (Int, Int) -> Square
getSquare board position = 
  let
      (rowIndex, columnIndex) = position
      maybeCurrentRow = Array.get rowIndex board
      currentRow = 
        case maybeCurrentRow of 
          Just squareRow ->
            squareRow
          Nothing ->
            --TODO see of there is a better way to handle this
            Array.fromList [ErrorSquare]
      maybeCurrentSquare = 
        Array.get columnIndex currentRow
      currentSquare = 
        case maybeCurrentSquare of
          Just square ->
            square
          Nothing ->
            ErrorSquare
  in
    currentSquare

--Update

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  let
      --TODO REMove me 
    _ = Debug.log "msg me" msg 
  in
  case msg of
    Reset ->
      ( 
        { board = defaultBoard
        , currentPlayer = White
        , activePiece = model.activePiece
      }, Cmd.none)
    
    Error ->
      ( 
        { board = defaultBoard
        , currentPlayer = White
        , activePiece = model.activePiece
      }, Cmd.none)
    
    SelectPiece unit unitPosition ->
      ( 
        { board = model.board
        , currentPlayer = White
        , activePiece = { unit = Just unit , unitPosition = Just unitPosition }
      }, Cmd.none)
    
    DeselectPiece ->
      ( 
        { board = model.board
        , currentPlayer = White
        , activePiece = { unit = Nothing, unitPosition = Nothing }
      }, Cmd.none)

    MoveUnit positionFrom positionTo unit ->
      let 
        (fromRowIndex, fromColumnIndex) = positionFrom
        oldFromRow = 
          case Array.get fromRowIndex model.board of
            Just row ->
              row
            Nothing ->
              Array.repeat 8 ErrorSquare
        newFromRow = 
          Array.set fromColumnIndex Unoccupied oldFromRow 
        (toRowIndex, toColumnIndex) = positionTo
        oldToRow = 
          case Array.get toRowIndex model.board of
            Just row ->
              row
            Nothing -> 
              Array.repeat 8 ErrorSquare
        newToRow = 
          Array.set toColumnIndex (Occupied unit) oldToRow

        startMoveBoard = Array.set fromRowIndex newFromRow model.board
        endMoveBoard = Array.set toRowIndex newToRow startMoveBoard
      in
        ( 
          { board = endMoveBoard
          , currentPlayer = changePlayer Black --TODO uncomment me model.currentPlayer
          , activePiece = { unit = Nothing, unitPosition = Nothing }
        }, Cmd.none)

    DoNothing ->
      ( model , Cmd.none)





--View

view : Model -> Document Msg
view model =
  { title = "Elm-Chess"
  , body = 
    [ h1 [] [ text "Elm Chess"] 
      , div [class "game-content"] (range 0 7 |> List.map (viewBoardRow model)) 
    ] }


viewBoardRow : Model -> Int -> Html Msg 
viewBoardRow model index =
    div [ class "row" ] (range 0 7 |> List.map (viewBoardButton model index))


viewBoardButton : Model -> Int -> Int -> Html Msg 
viewBoardButton model rowIndex columnIndex =
  let 
      board = model.board
      player = model.currentPlayer
      currentSquare = getSquare board (rowIndex, columnIndex)
      activePiecePosition = getActivePiecePostion model

      currentSquarePositionString = "square-" ++ (String.fromInt rowIndex) ++ "," ++ (String.fromInt columnIndex)
      squareColorString = choseBoardSquareColor (rowIndex, columnIndex)
      possibleLzs = 
        case model.activePiece.unit of
          Nothing ->
            Array.empty
          Just unit ->
            getPossibleLzs (getUnitMovementOptions board unit activePiecePosition) activePiecePosition
      lzString = 
        if Array.toList possibleLzs |> List.member (rowIndex, columnIndex) then
          " possible_lz"
        else
          ""
      activePieceString = 
        if activePiecePosition == (rowIndex, columnIndex) then
          " active_piece"
        else 
          ""
      classString = "square " 
        ++ currentSquarePositionString 
        ++ activePieceString 
        ++ squareColorString 
        ++ lzString
  in
    div
      [ class classString
      , createSquareOnclick model (rowIndex, columnIndex) |> onClick ]
      [ currentSquare |> viewBoardButtonText |> text ]


createSquareOnclick : Model -> (Int, Int) -> Msg
createSquareOnclick model currentSquarePosition = 
  let 
      square = getSquare model.board currentSquarePosition
      currentPlayer = model.currentPlayer
      activePiecePosition = getActivePiecePostion model
      possibleLzs = 
        case model.activePiece.unit of
          Nothing ->
            Array.empty
          Just unit ->
            getPossibleLzs (getUnitMovementOptions model.board unit activePiecePosition) activePiecePosition
  in
    case square of 
      Occupied unit ->
        case unit of
          Pawn owner ->
            case owner of
              Black ->
                case currentPlayer of
                  Black ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Pawn Black) currentSquarePosition
                    else 
                      DeselectPiece
                  White ->
                    --TODO
                    --if checkPath from 
                    --to move into function?
                    case model.activePiece.unit of
                      Just activeUnit ->
                        if Array.toList possibleLzs |> List.member currentSquarePosition then
                          MoveUnit activePiecePosition currentSquarePosition activeUnit
                        else 
                          DoNothing
                      Nothing ->
                        DoNothing
              White ->
                case currentPlayer of
                  Black ->
                    DoNothing
                  White ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Pawn White) currentSquarePosition
                    else 
                      DeselectPiece
          Rook owner -> 
            case owner of
              Black ->
                case currentPlayer of
                  Black ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Rook Black) currentSquarePosition
                    else 
                      DeselectPiece
                  White ->
                    DoNothing
              White ->
                case currentPlayer of
                  Black ->
                    DoNothing
                  White ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Rook White) currentSquarePosition
                    else 
                      DeselectPiece
          Knight owner ->
            case owner of
              Black ->
                case currentPlayer of
                  Black ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Knight Black) currentSquarePosition
                    else 
                      DeselectPiece
                  White ->
                    DoNothing
              White ->
                case currentPlayer of
                  Black ->
                    DoNothing
                  White ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Knight White) currentSquarePosition
                    else 
                      DeselectPiece
          Bishop owner ->
            case owner of
              Black ->
                case currentPlayer of
                  Black ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Bishop Black) currentSquarePosition
                    else 
                      DeselectPiece
                  White ->
                    DoNothing
              White ->
                case currentPlayer of
                  Black ->
                    DoNothing
                  White ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Bishop White) currentSquarePosition
                    else 
                      DeselectPiece
          Queen owner ->
            case owner of
              Black ->
                case currentPlayer of
                  Black ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Queen Black) currentSquarePosition
                    else 
                      DeselectPiece
                  White ->
                    DoNothing
              White ->
                case currentPlayer of
                  Black ->
                    DoNothing
                  White ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (Queen White) currentSquarePosition
                    else 
                      DeselectPiece
          King owner ->
            case owner of
              Black ->
                case currentPlayer of
                  Black ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (King Black) currentSquarePosition
                    else 
                      DeselectPiece
                  White ->
                    DoNothing
              White ->
                case currentPlayer of
                  Black ->
                    DoNothing
                  White ->
                    if currentSquarePosition /= activePiecePosition then
                      SelectPiece (King White) currentSquarePosition
                    else 
                      DeselectPiece
          ErrorUnit ->
            Error
      Unoccupied ->
        --TODO
        --if checkPath from 
        --to move into function?
        case model.activePiece.unit of
          Just unit ->
            if Array.toList possibleLzs |> List.member currentSquarePosition then
              MoveUnit activePiecePosition currentSquarePosition unit
            else 
              DoNothing
          Nothing ->
            DoNothing
      ErrorSquare ->
        Error  



viewBoardButtonText : Square -> String
viewBoardButtonText square =
      case square of
        Occupied unit ->
          case unit of
            Pawn owner ->
              case owner of
                Black ->
                  "B-P"
                White ->
                  "W-P"
            Rook owner -> 
              case owner of
                Black ->
                  "B-R"
                White ->
                  "W-R"
            Knight owner ->
              case owner of
                Black ->
                  "B-K"
                White ->
                  "W-K"
            Bishop owner ->
              case owner of
                Black ->
                  "B-B"
                White ->
                  "W-B"
            Queen owner ->
              case owner of
                Black ->
                  "B-Q"
                White ->
                  "W-Q"
            King owner ->
              case owner of
                Black ->
                  "B-King"
                White ->
                  "W-King"
            ErrorUnit ->
              "UNIT_ERROR"
        Unoccupied ->
          ""
        ErrorSquare ->
          "SQUARE_ERROR"


choseBoardSquareColor : (Int, Int) -> String
choseBoardSquareColor (rowIndex, columnIndex) = 
  if (isEven rowIndex) then
    if (isEven columnIndex) then
      " black_square"
    else
      " white_square"
  else 
    if (isEven columnIndex) then
      " white_square"
    else
      " black_square"




