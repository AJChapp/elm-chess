# Elm Chess
This document will be the planning of the building of this app.

## The Board

Currently using:
Board = Array 
    [ Array[Square, Square, Square, Square, Square, Square, Square, Square]
    , Array[Square, Square, Square, Square, Square, Square, Square, Square]
    , Array[Square, Square, Square, Square, Square, Square, Square, Square]
    , Array[Square, Square, Square, Square, Square, Square, Square, Square]
    , Array[Square, Square, Square, Square, Square, Square, Square, Square]
    , Array[Square, Square, Square, Square, Square, Square, Square, Square]
    , Array[Square, Square, Square, Square, Square, Square, Square, Square]
    , Array[Square, Square, Square, Square, Square, Square, Square, Square]
    ]

Possible future type:
record {
    a: Array[Square, Square, Square, Square, Square, Square, Square, Square]
    b: Array[Square, Square, Square, Square, Square, Square, Square, Square]
    c: Array[Square, Square, Square, Square, Square, Square, Square, Square]
    d: Array[Square, Square, Square, Square, Square, Square, Square, Square]
    e: Array[Square, Square, Square, Square, Square, Square, Square, Square]
    f: Array[Square, Square, Square, Square, Square, Square, Square, Square]
    g: Array[Square, Square, Square, Square, Square, Square, Square, Square]
    h: Array[Square, Square, Square, Square, Square, Square, Square, Square]
}

record {
    (0,1): Square
    (0,2): Square
    (0,3): Square
    ...
}

A chess board is 8 x 8 squares
I think i will implement as one array on the model of 64 squareValues
#### UPDATE: 
I think i am going to have to use 8 arrays of 8. This is to get an easy way to check if a piece would move off the board


## Piece Movement

### Triggering Movement
When you click on a piece it will become the "active piece". When this happend you will be able to see all of its available moves. The spaces that it can move to will blue unless there is an enemy in that space. In that case, the square will be red. 

Hint to acomplish
    - Functionally: None

    - Visually: 
        div [ class "square black" ++ (generateClasses model)] []
        the generateClasses function will return a string based on some complex logic (TBD) that will output some classes
## Declaring a Winner
    Everytime a unit is moved a check will be made on every one of the current players pieces. If the opposing kings position is in the collective moveList of all of the current players pieces the opposing player is put in check. If the king gets taken after that it is checkmate. 

I decided against doing any sort of check for checkmate because to me a player should have to take the king. (also doing a check for checkmate would be very expensive.) I will most likly go back and add the functionality if i have the time.


### Actual Movement
I should be able to give a piece a "movement value".
  - Ex: (-1,3) which would be three block forward and one block to the left
  - Update: I reversed it so it will be a (y,x) pair instead of (x,y)

Im not 100% how im going to implement a piece not being able to move thru another piece but im thinking that when a piece moves it does just appear on that tile. Instead it would check each step along the way. 
  - Ex: a bishop moving that wants to move (-3,3) would first check the tile at (-1,1) then (-2,2) then (-3, 3) if there is a piece at (-2,2) it should not be able to move to that square.

## Piece Properties 
* Pawn 
  * Moves up to two squares on first move then one after that
  * Can attack diagonally
  * Special moves: En Passant & promotion 
* Rook
  * Moves any number of spaces vertically or horizontally
  * Special moves: Castling
* Knight 
  * Moves in L shape (2,-1) (2,1) (-1,2) (1,2) (-2,1) (-2,-1) (1,-2) (-1,-2)
  * Can jump over enemies
* Bishop 
  * can move diagonal in any direction. (1,1) (2,2) ...
* Queen 
  * can move vertically, horizontal, diagonally any number of spaces
* King 
  * can move vertically, horizontal, diagonally one space

(This might be usefule for styling: https://www.chess.com/article/view/chess-board-dimensions)

## Development Phases 
Note: there will heavy testing bewteen phases
### Phase 1a.) (complete) 
 * All chess pieces can move and take pieces
   * No special moves 
   * Winner declared when king is taken

### Phase 1b.) (complete)
 * Test

### Phase 2a.) 
 * Special moves
   * En Passant
   * Castling
   * Unit Promotions
 * When checked, you get an alert of some kind
 * Implement Tie

### Phase 2b.) 
 * Test

### Phase 3a.)
 * Install script
 * Uninstall Script
 * Show taken pieces
 * Move timer
 * Unit Graphics

### Phase 3b.)
 * Test 
