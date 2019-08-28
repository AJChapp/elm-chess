# Elm Chess
This document will be the planning of the building of this app.

## The Board

A chess board is 8 x 8 squares
I think i will implement as one array on the model of 64 squareValues


## Piece Movement

### Triggering Movement
When you click on a piece it will become the "active piece". When this happend you will be able to see all of its available moves. The spaces that it can move to will blue unless there is an enemy in that space. In that case, the square will be red. 

Hint to acomplish
    - Functionally: 
        None

    - Visually: 
        div [ class "square black" ++ (generateClasses model)] []
        the generateClasses function will return a string based on some complex logic (TBD) that will output some classes

### Actual Movement
I should be able to give a piece a "movement value".
  - Ex: (3, -1) which would be three block forward and one block to the left

Im not 100% how im going to implement a piece not being able to move thru another piece but im thinking that when a piece moves it does just appear on that tile. Instead it would check each step along the way. 
  - Ex: a bishop moving that wants to move (3,-3) would first check the tile at (1,-1) then (2,-2) then (3, -3) if there is a piece at (2,-2) it should not be able to move to that square.

## Piece Properties 
* Pawn 
* * Moves


(This might be usefule for styling: https://www.chess.com/article/view/chess-board-dimensions)
