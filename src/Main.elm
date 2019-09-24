module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import List
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
  --| ErrorUnit

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


getUnitOwner : Unit -> Player 
getUnitOwner unit =
  case unit of 
    Pawn player ->
      player
    Rook player ->
      player
    Knight player ->
      player
    Bishop player ->
      player
    Queen player ->
      player
    King player ->
      player


----------------------------------------------------------------------------------------------------------------------
---- Unit Moves ------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------

-- Pawn --
generatePawnMoveList : GameBoard -> Player -> (Int, Int) -> Array (Int, Int)
generatePawnMoveList board owner (rowIndex, columnIndex) =
  case owner of
    Black ->
      let
        baseMoveList = Array.fromList [ (1, 0) ]
        moveList_v2 = 
          if rowIndex == 1 then
            --TODO implement En Passant
            --TODO implement check to see if there is a unit at (1, 0)
            Array.fromList [ (2, 0) ] |> Array.append baseMoveList 
          else
            baseMoveList
        moveList_v3 = 
          if isPositionValid (rowIndex + 1, columnIndex + 1) then 
            case getSquare board (rowIndex + 1, columnIndex + 1) of
              Occupied unit ->
                case (getUnitOwner unit) of 
                  Black ->
                    moveList_v2
                  White ->
                    Array.fromList [ (1, 1) ] |> Array.append moveList_v2
              _ ->
                moveList_v2
          else
            moveList_v2
        moveList_v4 = 
          if isPositionValid (rowIndex + 1, columnIndex - 1) then
            case getSquare board (rowIndex + 1, columnIndex + 1) of
              Occupied unit ->
                case (getUnitOwner unit) of 
                  Black ->
                    moveList_v3
                  White ->
                    Array.fromList [ (1, -1) ] |> Array.append moveList_v3
              _ ->
                moveList_v3
          else 
            moveList_v3
      in 
        getPossibleLzs (rowIndex, columnIndex) moveList_v4
    White ->
      let
          --TODO rename moveLists
        baseMoveList = 
          if isPositionValid (rowIndex - 1, columnIndex) then
           case getSquare board (rowIndex - 1, columnIndex) of 
            Occupied unit ->
              Array.empty 
            Unoccupied ->
              Array.fromList [ (-1, 0) ]
            ErrorSquare -> 
              Array.empty
          else 
            Array.empty 

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
                case (getUnitOwner unit) of 
                  White -> 
                    moveList_v2
                  Black ->
                    Array.fromList [ (-1, 1) ] |> Array.append moveList_v2 
              _ ->
                moveList_v2
          else
            moveList_v2

        moveList_v4 = 
          if isPositionValid (rowIndex - 1, columnIndex - 1) then 
            case getSquare board (rowIndex - 1, columnIndex - 1) of
              Occupied unit ->
                case (getUnitOwner unit) of 
                  White -> 
                    moveList_v3
                  Black ->
                    Array.fromList [ (-1, -1) ] |> Array.append moveList_v3
              _ ->
                moveList_v3
          else
            moveList_v3
      in
        getPossibleLzs (rowIndex, columnIndex) moveList_v4


-- Rook --
generateRookMoveList : GameBoard -> Player -> (Int, Int) -> Array (Int, Int)
generateRookMoveList board owner (rowIndex, columnIndex) =
  let 
    moveArray_v1 = 
      Array.initialize 8 (\(y) -> (y, 0))  
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenVertical board (rowIndex, columnIndex))
    moveArray_v2 = 
      Array.initialize 8 (\(y) -> (y * -1, 0))  
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenVertical board (rowIndex, columnIndex))
    moveArray_v3 = 
      Array.initialize 8 (\(x) -> (0, x))  
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenHorizontal board (rowIndex, columnIndex))
    moveArray_v4 = 
      Array.initialize 8 (\(x) -> (0, x * -1))  
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenHorizontal board (rowIndex, columnIndex))
   
    finalMoveArray =
      Array.append moveArray_v1 moveArray_v2
        |> Array.append moveArray_v3
        |> Array.append moveArray_v4
  in
    finalMoveArray


-- Unit Move Index -- 
getUnitMovementOptions : GameBoard -> Unit -> (Int, Int) -> Array (Int, Int)
getUnitMovementOptions board unit unitPosition =
  case unit of
    Pawn owner ->
      generatePawnMoveList board owner unitPosition 
    Rook owner -> 
      generateRookMoveList board owner unitPosition
    Knight owner ->
      let 
        --TODO can i generate this?
        moveList = Array.fromList [ (2,-1), (2,1), (-1,2), (1,2), (-2,1), (-2,-1), (1,-2), (-1,-2) ]
      in 
        getPossibleLzs unitPosition moveList
    Bishop owner ->
      Array.fromList [ (1,2) ]
    Queen owner ->
      Array.fromList [ (1,2) ]
    King owner ->
      Array.fromList [ (1,2) ]

checkSpaceBewteenVertical : GameBoard -> (Int, Int) -> (Int, Int) -> Bool
checkSpaceBewteenVertical board (currentY, currentX) (newY, newX) =
  let
    (lowY,highY) =
      if currentY >= newY then
        (newY,currentY)
      else
        (currentY, newY)

    invalidSquares =
      List.range (lowY + 1) (highY - 1) -- Get y values for inbetween spaces
      |> List.map (\(y) -> (y, currentX)) -- Group with their x value
      |> List.map (\(y, x) -> getSquare board (y,x)) --Converts to squares
      |> List.filter (\ (square) -> 
        case square of
          Unoccupied ->
            False
          _ ->
            True
      ) 
  in
      if (List.length invalidSquares) > 0 then
        False
      else
        True

checkSpaceBewteenHorizontal : GameBoard -> (Int, Int) -> (Int, Int) -> Bool
checkSpaceBewteenHorizontal board (currentY, currentX) (newY, newX) =
  let
    (lowX,highX) =
      if currentX >= newX then
        (newX,currentX)
      else
        (currentX, newX)

    invalidSquares =
      List.range (lowX + 1) (highX - 1) -- Get x values for inbetween spaces
      |> List.map (\(x) -> (currentY, x)) -- Group with their y value
      |> List.map (\(y, x) -> getSquare board (y,x)) --Converts to squares
      |> List.filter (\ (square) -> 
        case square of
          Unoccupied ->
            False
          _ ->
            True
      )
  in
    if (List.length invalidSquares) > 0 then
      False
    else
      True


isPositionValid : (Int, Int) -> Bool
isPositionValid (yIndex, xIndex) =
  if yIndex < 0 || yIndex > 7 || xIndex < 0 || xIndex > 7 then
    False 
  else 
    True


getPossibleLzs : (Int, Int) -> Array (Int, Int) -> Array (Int, Int)
getPossibleLzs currentPosition possibleMoves =
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


--TODO See if there is a better way to do this
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
      --TODO Remove me 
    _ = Debug.log "MSG" msg 
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
        _ = Debug.log "oldFromRow" oldFromRow
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

        startMoveBoard =  Array.set fromRowIndex newFromRow model.board
        endMoveBoard = 
          if (isPositionValid positionFrom) && (isPositionValid positionTo) then
            Array.set toRowIndex newToRow startMoveBoard
          else
            let
              _ = Debug.log "Invalid Value Passed MoveUnit (positionFrom, positionTo)" (positionFrom, positionTo)
            in
              model.board
        nextPlayer = 
          if (isPositionValid positionFrom) && (isPositionValid positionTo) then
            changePlayer model.currentPlayer
          else
            model.currentPlayer
      in
        ( 
          { board = endMoveBoard
          , currentPlayer = changePlayer Black --TODO nextPlayer
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
      , div [class "game-content"] (List.range 0 7 |> List.map (viewBoardRow model)) 
    ] }


viewBoardRow : Model -> Int -> Html Msg 
viewBoardRow model index =
    div [ class "row" ] (List.range 0 7 |> List.map (viewBoardButton model index))


viewBoardButton : Model -> Int -> Int -> Html Msg 
viewBoardButton model rowIndex columnIndex =
  let 
      --TODO Clean this up
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
            getUnitMovementOptions board unit activePiecePosition 
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
            getUnitMovementOptions model.board unit activePiecePosition 
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
          --ErrorUnit ->
            --Error
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
            --ErrorUnit ->
              --"UNIT_ERROR"
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




