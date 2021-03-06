module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser
import List
import Html exposing (Html, button, div, h1, h2, text, h4)
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

type Unit
  = Pawn Player
  | Rook Player
  | Knight Player
  | Bishop Player
  | Queen Player
  | King Player

type Status 
  = InProgress
  | Winner Player
  | Tie

type alias Model =
  { board : GameBoard
  , currentPlayer : Player
  , gameStatus: Status
  , activePiece : 
    { unit: Maybe Unit 
    , unitPosition: Maybe (Int, Int) }
  }

type Msg
  = Reset
  | SelectPiece Unit (Int, Int)
  | DeselectPiece
  | MoveUnit (Int, Int) (Int,Int) Unit
  | DoNothing

init : () -> ( Model, Cmd Msg )
init _ = 
  (intialModel, Cmd.none)


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
generatePawnMoveArray : GameBoard -> Player -> (Int, Int) -> Array (Int, Int)
generatePawnMoveArray board owner (rowIndex, columnIndex) =
  let
    -- Pawns can only move forward. However forward is relative to the player. I use decider "decides" what direction is forward
    decider = 
      case owner of
        Black -> 
          1
        White ->
          -1

    moveArray_v1 = 
      case getSquare board (rowIndex + 1 * decider, columnIndex) of
        Just square ->
          case square of
            Occupied unit ->
              if getUnitOwner unit == owner then 
                Array.fromList [ (1 * decider, 0) ]
              else
                Array.empty
            Unoccupied ->
              Array.fromList [ (1 * decider, 0) ]
        Nothing ->
          Array.empty
    moveArray_v2 = 
      if rowIndex == 1 && decider == 1 || rowIndex == 6 && decider == -1 then
        if checkSpaceBewteenVertical board (rowIndex, columnIndex) (rowIndex + 2 * decider, columnIndex) then
          if (getSquare board (rowIndex + 2 * decider, columnIndex)) == Just Unoccupied then
            Array.fromList [ (2 * decider, 0) ]
          else
            Array.empty
        else 
          Array.empty
      else
        Array.empty
    moveArray_v3 = 
      if doesPositionExist (rowIndex + 1 * decider, columnIndex + 1) then 
        case getSquare board (rowIndex + 1 * decider, columnIndex + 1) of
          Just square ->
            case square of
              Occupied unit ->
                if (getUnitOwner unit) /= owner then 
                  Array.fromList [ (1 * decider, 1) ]
                else
                  Array.empty
              Unoccupied ->
                Array.empty
          Nothing ->
            Array.empty
      else
        Array.empty
    moveArray_v4 = 
      if doesPositionExist (rowIndex + 1 * decider, columnIndex - 1) then
        case getSquare board (rowIndex + 1 * decider, columnIndex - 1) of
          Just square ->
            case square of
              Occupied unit ->
                if (getUnitOwner unit) /= owner then 
                  Array.fromList [ (1 * decider, -1) ]
                else
                  Array.empty
              Unoccupied ->
                Array.empty
          Nothing ->
            Array.empty
      else 
        Array.empty

    finalMoveArray = Array.append moveArray_v1 moveArray_v2
        |> Array.append moveArray_v3
        |> Array.append moveArray_v4
  in 
    getPossibleLzs (rowIndex, columnIndex) finalMoveArray


-- Rook --
generateRookMoveArray : GameBoard -> (Int, Int) -> Array (Int, Int)
generateRookMoveArray board (rowIndex, columnIndex) =
  let 
    moveArray_v1 = Array.initialize 8 (\(y) -> (y, 0))  
        |> Array.append (Array.initialize 8 (\(y) -> (y * -1, 0)))
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenVertical board (rowIndex, columnIndex))
    moveArray_v2 = Array.initialize 8 (\(x) -> (0, x))  
        |> Array.append (Array.initialize 8 (\(x) -> (0, x * -1)))
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenHorizontal board (rowIndex, columnIndex))
  in
    Array.append moveArray_v1 moveArray_v2


-- Bishop --
generateBishopMoveArray : GameBoard -> (Int, Int) -> Array (Int, Int)
generateBishopMoveArray board (rowIndex, columnIndex) =
  let
    moveArray = 
      Array.initialize 8 (\(x) -> (x, x))
        |> Array.append (Array.initialize 8 (\(x) -> (x * -1, x)))
        |> Array.append (Array.initialize 8 (\(x) -> (x, x * -1)))
        |> Array.append (Array.initialize 8 (\(x) -> (x * -1, x * -1)))
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenDiagonal board (rowIndex, columnIndex))
  in
    moveArray


-- Queen --
generateQueenMoveArray: GameBoard -> (Int, Int) -> Array (Int, Int)
generateQueenMoveArray board (rowIndex, columnIndex) =
  let
    moveArray_v1 = 
      Array.initialize 8 (\(x) -> (x, x))
        |> Array.append (Array.initialize 8 (\(x) -> (x * -1, x)))
        |> Array.append (Array.initialize 8 (\(x) -> (x, x * -1)))
        |> Array.append (Array.initialize 8 (\(x) -> (x * -1, x * -1)))
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenDiagonal board (rowIndex, columnIndex))
    moveArray_v2 = Array.initialize 8 (\(y) -> (y, 0))  
        |> Array.append (Array.initialize 8 (\(y) -> (y * -1, 0)))
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenVertical board (rowIndex, columnIndex))
    moveArray_v3 = Array.initialize 8 (\(x) -> (0, x))  
        |> Array.append (Array.initialize 8 (\(x) -> (0, x * -1)))
        |> getPossibleLzs (rowIndex, columnIndex)
        |> Array.filter (checkSpaceBewteenHorizontal board (rowIndex, columnIndex))
   
    finalMoveArray = Array.append moveArray_v1 moveArray_v2
        |> Array.append moveArray_v3
  in
    finalMoveArray

-- Unit Move Index -- 
getUnitMovementOptions : GameBoard -> Maybe Unit -> Maybe (Int, Int) -> Array (Int, Int)
getUnitMovementOptions board maybeUnit maybeUnitPosition =
  case maybeUnitPosition of
    Just unitPosition ->
      case maybeUnit of
        Just unit ->
          case unit of
            Pawn owner ->
              generatePawnMoveArray board owner unitPosition 
            Rook owner -> 
              generateRookMoveArray board unitPosition
            Knight owner ->
              let 
                moveArray = Array.fromList [ (2,-1), (2,1), (-1,2), (1,2), (-2,1), (-2,-1), (1,-2), (-1,-2) ]
              in 
                getPossibleLzs unitPosition moveArray
            Bishop owner ->
              generateBishopMoveArray board unitPosition
            Queen owner ->
              generateQueenMoveArray board unitPosition
            King owner ->
              let
                  moveArray = Array.fromList [ (-1,0), (-1,1), (0,1), (1,1), (1,0), (-1,-1), (0,-1), (1,-1), (0,0)]
              in 
                  getPossibleLzs unitPosition moveArray
        Nothing ->
          Array.empty
    Nothing ->
      Array.empty

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
      |> List.filter (\ (maybeSquare) -> 
        case maybeSquare of
          Just square ->
            case square of
              Unoccupied ->
                False
              Occupied unit ->
                True
          Nothing ->
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
      |> List.filter (\ (maybeSquare) -> 
        case maybeSquare of
          Just square ->
            case square of 
              Unoccupied ->
                False
              Occupied unit ->
                True
          Nothing ->
            True
      )
  in
    if (List.length invalidSquares) > 0 then
      False
    else
      True


checkSpaceBewteenDiagonal : GameBoard -> (Int, Int) -> (Int, Int) -> Bool
checkSpaceBewteenDiagonal board (currentY, currentX) (newY, newX) =
  let
    (lowX,highX) =
      if currentX >= newX then
        (newX, currentX)
      else
        (currentX, newX)

    (lowY,highY) =
      if currentY >= newY then
        (newY, currentY)
      else
        (currentY, newY)

    yValues = -- List of all the yValues of inbetween squares
      if currentY >= newY then
          List.range (lowY + 1) (highY - 1)
        else 
          List.range (lowY + 1) (highY - 1)
          |> List.reverse
    xValues = -- List of all the xValues of inbetween squares
      if currentX >= newX then 
        List.range (lowX + 1) (highX - 1)
      else 
        List.range (lowX + 1) (highX - 1)
        |> List.reverse

    invalidSquares =
      squashIntListToTupleList yValues xValues --Combines Y's with their X's
        |> List.map (\(y, x) -> getSquare board (y, x)) --Converts to squares
        |> List.filter (\ (maybeSquare) -> -- Filters out all of the valid squares
          case maybeSquare of
            Just square ->
              case square of 
                Unoccupied ->
                  False
                Occupied unit ->
                  True
            Nothing ->
              True
        )
  in
    if (List.length invalidSquares) > 0 then -- if there are any invalid squares the move is invalid
      False
    else
      True

squashIntListToTupleList : List Int -> List Int -> List(Int,Int)
squashIntListToTupleList yList xList =
  let
    yArray = Array.fromList yList
    xArray = Array.fromList xList
  in
    Array.indexedMap (\ i y -> (y, (Maybe.withDefault 10 (Array.get i xArray)))) yArray
      |> Array.filter (\ (y, x) -> 
        if (x > 9) then
          False
        else
          True
        )
      |> Array.toList


doesPositionExist : (Int, Int) -> Bool
doesPositionExist (yIndex, xIndex) =
  if yIndex < 0 || yIndex > 7 || xIndex < 0 || xIndex > 7 then
    False 
  else 
    True


getPossibleLzs : (Int, Int) -> Array (Int, Int) -> Array (Int, Int)
getPossibleLzs currentPosition possibleMoves =
  let 
    (rowIndex, columnIndex) = currentPosition
    possibleCoordinates = Array.map (\(y, x) -> (y + rowIndex, x + columnIndex)) possibleMoves
  in  
    Array.filter doesPositionExist possibleCoordinates


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


getSquare : GameBoard -> (Int, Int) -> Maybe Square
getSquare board position = 
  let
      (rowIndex, columnIndex) = position
      maybeCurrentRow = Array.get rowIndex board
      currentRow = 
        case maybeCurrentRow of 
          Just squareRow ->
            squareRow
          Nothing ->
            Array.empty
      maybeCurrentSquare = 
        Array.get columnIndex currentRow
  in
    maybeCurrentSquare

--Update

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      (intialModel, Cmd.none)
    
    SelectPiece unit unitPosition ->
       ( { model | activePiece = { unit = Just unit, unitPosition = Just unitPosition } }, Cmd.none )
    
    DeselectPiece ->
       ( { model | activePiece = { unit = Nothing, unitPosition = Nothing } }, Cmd.none )

    MoveUnit positionFrom positionTo unit ->
      let 
        --TODO clean this up
        (fromRowIndex, fromColumnIndex) = positionFrom

        oldFromRow = 
          case Array.get fromRowIndex model.board of
            Just row ->
              row
            Nothing ->
              Array.empty

        newFromRow = 
          Array.set fromColumnIndex Unoccupied oldFromRow 

        startMoveBoard =  Array.set fromRowIndex newFromRow model.board

        (toRowIndex, toColumnIndex) = positionTo

        oldToRow = 
          case Array.get toRowIndex startMoveBoard of
            Just row ->
              row
            Nothing -> 
              Array.empty

        newToRow = 
          Array.set toColumnIndex (Occupied unit) oldToRow

        (endMoveBoard, nextPlayer) = 
          if (doesPositionExist positionFrom) && (doesPositionExist positionTo) then
            ((Array.set toRowIndex newToRow startMoveBoard), (changePlayer model.currentPlayer))
          else
            (model.board, model.currentPlayer)
      in
        if (getSquare model.board positionTo) == (Just <| Occupied <| King nextPlayer) then
          ( 
            { board = endMoveBoard
            , currentPlayer = model.currentPlayer
            , gameStatus = Winner model.currentPlayer
            , activePiece = { unit = Nothing, unitPosition = Nothing }
          }, Cmd.none)
        else
          ( 
            { board = endMoveBoard
            , currentPlayer = nextPlayer 
            , gameStatus = InProgress
            , activePiece = { unit = Nothing, unitPosition = Nothing }
          }, Cmd.none)

    DoNothing ->
      ( model , Cmd.none)
 


--View

view : Model -> Document Msg
view model =
  { title = "Elm-Chess"
  , body = 
    [ div [ class "header"] 
      [ h1 [ class "title"] [ text "Smart Chess"] 
      , h2 [ class "game-status"] [ text (createStatusText model.gameStatus) ]
      , h4 [ class "current-player"] [ text ("Current Player: " ++ (playerToString model.currentPlayer))]
      , button [ class "reset-btn", onClick Reset] [ text "Reset"]
      ]
    , div [class ("game-content " ++ playerToString model.currentPlayer)] (List.range 0 7 |> List.map (viewBoardRow model)) 
    ] }


viewBoardRow : Model -> Int -> Html Msg 
viewBoardRow model index =
    div [ class "row" ] (List.range 0 7 |> List.map (viewBoardButton model index))


viewBoardButton : Model -> Int -> Int -> Html Msg 
viewBoardButton model rowIndex columnIndex =
  let 
      currentSquare = getSquare model.board (rowIndex, columnIndex)

      possibleLzs = 
        getUnitMovementOptions model.board model.activePiece.unit model.activePiece.unitPosition

      classString = createClassString (rowIndex, columnIndex) possibleLzs model
  in
    case currentSquare of
      Just square ->
        div
          [ class classString
          , createSquareOnclick model (rowIndex, columnIndex) |> onClick ]
          [ square |> viewBoardButtonText |> text ]
      Nothing ->
        Html.text ""


createClassString : (Int, Int) -> Array (Int,Int) -> Model -> String
createClassString (rowIndex, columnIndex) possibleLzs model =
  let
      squareColorString = choseBoardSquareColor (rowIndex, columnIndex)
      currentSquarePositionString = " square-" ++ (String.fromInt rowIndex) ++ "," ++ (String.fromInt columnIndex)
      lzString = 
        if Array.toList possibleLzs |> List.member (rowIndex, columnIndex) then
          " possible_lz"
        else
          ""
      activePieceString = 
        if model.activePiece.unitPosition == Just (rowIndex, columnIndex) then
          " active_piece"
        else 
          ""

      classString = "square " 
        ++ squareColorString 
        ++ currentSquarePositionString 
        ++ lzString
        ++ activePieceString 
  in
    classString


createSquareOnclick : Model -> (Int, Int) -> Msg
createSquareOnclick model currentSquarePosition = 
  let 
      square = getSquare model.board currentSquarePosition
      currentPlayer = model.currentPlayer
      possibleLzs = 
        getUnitMovementOptions model.board model.activePiece.unit model.activePiece.unitPosition
  in
    case model.gameStatus of
      Tie -> 
        DoNothing
      Winner player -> 
        DoNothing
      InProgress ->
        case square of 
          Just validSquare ->
            case validSquare of
                Occupied unit ->
                  let 
                    unitOwner = getUnitOwner unit
                  in 
                    if unitOwner == currentPlayer then
                      case model.activePiece.unitPosition of 
                        Just activePiecePosition -> 
                          if currentSquarePosition /= activePiecePosition then
                            SelectPiece unit currentSquarePosition
                          else
                            DeselectPiece
                        Nothing ->
                          SelectPiece unit currentSquarePosition
                    else
                      case model.activePiece.unit of
                        Just activeUnit ->
                          if Array.toList possibleLzs |> List.member currentSquarePosition then
                            case model.activePiece.unitPosition of
                              Just activePiecePosition ->
                                MoveUnit activePiecePosition currentSquarePosition activeUnit
                              Nothing ->
                                DoNothing
                          else 
                            DoNothing
                        Nothing ->
                          DoNothing
                Unoccupied ->
                  case model.activePiece.unit of
                    Just activeUnit ->
                      if Array.toList possibleLzs |> List.member currentSquarePosition then
                        case model.activePiece.unitPosition of
                          Just activePiecePosition ->
                            MoveUnit activePiecePosition currentSquarePosition activeUnit
                          Nothing ->
                            DoNothing
                      else 
                        DoNothing
                    Nothing ->
                      DoNothing
          Nothing ->
            DoNothing


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
        Unoccupied ->
          ""


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


playerToString: Player -> String
playerToString player = 
  case player of
    Black ->
      "Black"
    White ->
      "White"


createStatusText: Status -> String
createStatusText status = 
  case status of
    Winner player ->
      String.toUpper (playerToString player) ++ " WINS!"
    Tie ->
      "TIE GAME!"
    InProgress ->
      "In Progress"


intialModel: Model
intialModel = 
  { board = defaultBoard
    , currentPlayer = White
    , gameStatus = InProgress
    , activePiece = 
      { unit = Nothing
      , unitPosition = Nothing }
  }


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
