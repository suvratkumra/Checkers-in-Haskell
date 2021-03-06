module Checkers.Moves where

import Checkers.Types




{-
    A function to check if a list is empty or not, (will be used for the jumps in moves)
    meaning that for the moves to implement, first we have to check if the jump moves are available, if yes then 
    the jump moves will be first executed and then something else
-}

available :: [a] -> Bool 
available [] = False
available xs = True


-- Implement your code for moves function below
moves:: GameState -> SMorJM [Move]
moves gs | available(jumps gs)  = JM (jumps gs)
         | otherwise = SM (simple_moves gs)

{-
    Jumpssssssssssssssssssssssssssssssssssssssssssssssssssss
-}

jumps :: GameState -> [Move]
jumps gs 
    | (status gs == RedPlayer) = allRedPawnJumps gs (redPieces gs) [] ++ (kingCollectedJumps gs (redKings gs) [])
    | (status gs == BlackPlayer) = allBlackPawnJumps gs (blackPieces gs) [] ++ kingCollectedJumps gs (blackKings gs) []
    | otherwise = []

{-
    collecting all the simple moves depending upon the type of player
-}
simple_moves :: GameState -> [Move]
simple_moves gs
        | status gs == RedPlayer = (redPawnMoves gs (redPieces gs) []) ++ (kingMoves gs (redKings gs) [])
        | status gs == BlackPlayer = (blackPawnMoves gs (blackPieces gs) []) ++ (kingMoves gs (blackKings gs) [])
        | otherwise = []


{-
    Just making a simple function which is used to check if the coordinates are available on the board or not, i.e shouldnt be above 7 or below 0
-}

isOnBoard :: Coord -> Bool
isOnBoard (x,y) | (x>=0) && (x<=7) && (y<=7) && (y>=0) = True
                | otherwise = False


-- Making the functions which will tell us if the surrounding diagonals of the piece are empty or not
-- will be used for the simple moves
-- we know that redpieces, blackPieces, blackKings, redKings, all are a part of GameState and we write any piece and then
-- GameState instance in front of it because we want all the coordinates of that piece and compare the next coordinate (depending on the move player wants)
-- and then check if that spot is vacant or not, if not then the move is not valid, if yes then the move is valid, when we say "anyPiece s" so that extracts the 
-- list of coordinates of that kind of piece
isUpLeftVacant :: GameState -> Coord -> Bool
isUpLeftVacant s (x,y) 
        | notElem(x-1, y-1) (redPieces s) && notElem(x-1, y-1) (blackPieces s) && notElem(x-1, y-1) (redKings s) && notElem(x-1, y-1) (blackKings s) && (x-1 >= 0) && (y-1 >= 0) = True
        | otherwise = False

isUpRightVacant :: GameState -> Coord -> Bool
isUpRightVacant s (x,y) 
        | notElem(x+1, y-1) (redPieces s) && notElem(x+1, y-1) (blackPieces s) && notElem(x+1, y-1) (redKings s) && notElem(x+1, y-1) (blackKings s) && (x+1 <= 7) && (y-1 >= 0) = True
        | otherwise = False

isDownRightVacant :: GameState -> Coord -> Bool
isDownRightVacant s (x,y)
        | notElem(x+1, y+1) (redPieces s) && notElem(x+1, y+1) (blackPieces s) && notElem(x+1, y+1) (redKings s) && notElem(x+1, y+1) (blackKings s) && (x+1 <= 7) && (y+1 <= 7) = True
        | otherwise = False

isDownLeftVacant :: GameState -> Coord -> Bool
isDownLeftVacant s (x,y)
        | notElem(x-1, y+1) (redPieces s) && notElem(x-1, y+1) (blackPieces s) && notElem(x-1, y+1) (redKings s) && notElem(x-1, y+1) (blackKings s) && (x-1 >= 0) && (y+1 <= 7) = True
        | otherwise = False






{-
    Now we will do the same thing we did above for the simple moves, now we will do it for the jump cases
    we will also add one more condition here when the status of the game is set to be red then we will also check
    if the diagonal of that peice is black then only we can jump and the jump is in the bounds of the board.
-}

isJumpDownRightVacant :: GameState -> Coord -> Bool
isJumpDownRightVacant s (x,y)
    | (status s == BlackPlayer) && (notElem(x+2, y+2) (redPieces s)) && (notElem(x+2, y+2) (blackPieces s)) && (notElem(x+2, y+2) (redKings s)) && (notElem(x+2, y+2) (blackKings s))
    && (isOnBoard ((x+2),(y+2)))             -- so that we dont go out of the board 
    && ((x+1, y+1) `elem` (redPieces s) || (x+1, y+1) `elem` (redKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | (status s == RedPlayer) && (notElem(x+2, y+2) (redPieces s)) && (notElem(x+2, y+2) (blackPieces s)) && (notElem(x+2, y+2) (redKings s)) && (notElem(x+2, y+2) (blackKings s))
    && (isOnBoard ((x+2),(y+2)))             -- so that we dont go out of the board 
    && ((x+1, y+1) `elem` (blackPieces s) || (x+1, y+1) `elem` (blackKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | otherwise = False
    
isJumpUpRightVacant :: GameState -> Coord -> Bool
isJumpUpRightVacant s (x,y)
    | (status s == BlackPlayer) && (notElem(x+2, y-2) (redPieces s)) && (notElem(x+2, y-2) (blackPieces s)) && (notElem(x+2, y-2) (redKings s)) && (notElem(x+2, y-2) (blackKings s))
    && (isOnBoard ((x+2),(y-2)))             -- so that we dont go out of the board 
    && ((x+1, y-1) `elem` (redPieces s) || (x+1, y-1) `elem` (redKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | (status s == RedPlayer) && (notElem(x+2, y-2) (redPieces s)) && (notElem(x+2, y-2) (blackPieces s)) && (notElem(x+2, y-2) (redKings s)) && (notElem(x+2, y-2) (blackKings s))
    && (isOnBoard ((x+2),(y-2)))             -- so that we dont go out of the board 
    && ((x+1, y-1) `elem` (blackPieces s) || (x+1, y-1) `elem` (blackKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | otherwise = False
    
isJumpUpLeftVacant :: GameState -> Coord -> Bool
isJumpUpLeftVacant s (x,y)
    | (status s == BlackPlayer) && (notElem(x-2, y-2) (redPieces s)) && (notElem(x-2, y-2) (blackPieces s)) && (notElem(x-2, y-2) (redKings s)) && (notElem(x-2, y-2) (blackKings s))
    && (isOnBoard ((x-2),(y-2)))             -- so that we dont go out of the board 
    && ((x-1, y-1) `elem` (redPieces s) || (x-1, y-1) `elem` (redKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | (status s == RedPlayer) && (notElem(x-2, y-2) (redPieces s)) && (notElem(x-2, y-2) (blackPieces s)) && (notElem(x-2, y-2) (redKings s)) && (notElem(x-2, y-2) (blackKings s))
    && (isOnBoard ((x-2),(y-2)))             -- so that we dont go out of the board 
    && ((x-1, y-1) `elem` (blackPieces s) || (x-1, y-1) `elem` (blackKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | otherwise = False
    
isJumpDownLeftVacant :: GameState -> Coord -> Bool
isJumpDownLeftVacant s (x,y)
    | (status s == BlackPlayer) && (notElem(x-2, y+2) (redPieces s)) && (notElem(x-2, y+2) (blackPieces s)) && (notElem(x-2, y+2) (redKings s)) && (notElem(x-2, y+2) (blackKings s))
    && (isOnBoard ((x-2),(y+2)))             -- so that we dont go out of the board 
    && ((x-1, y+1) `elem` (redPieces s) || (x-1, y+1) `elem` (redKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | (status s == RedPlayer) && (notElem(x-2, y+2) (redPieces s)) && (notElem(x-2, y+2) (blackPieces s)) && (notElem(x-2, y+2) (redKings s)) && (notElem(x-2, y+2) (blackKings s))
    && (isOnBoard ((x-2),(y+2)))             -- so that we dont go out of the board 
    && ((x-1, y+1) `elem` (blackPieces s) || (x-1, y+1) `elem` (blackKings s))      --for jumping we need the diagonal element to be of the opposite color, so checking that condition
    = True
    | otherwise = False
    
    
{-
    Okay logically if now the jump is successful so we now need to rove the piece from the PieceState as it contains the list of coord for all the pieces available at that time
    so we will make a simple function which roves coordinate of piece from the list and returns the list with that roved coordinate
-}

{-
    we know that the definition of filter is "	returns a list constructed from members of a list (the second argument) fulfilling a condition given by the first argument"
    so we will use that definition to refine the list, if the condition is fullfilled (x==a and y ==b) then the piece will be roved, if not then no
-}
    
rover :: Coord -> PieceState -> PieceState                --PieceState = [Coord]
rover _ [] = []
rover value list = filter (\x -> comparing value x) list 
    where 
        comparing (a,b) (x, y) | x == a && y == b = False
                               | otherwise = True
                           
{-
    Debugging what we have above, so suppose we have to remove (1,2) from [(1,2), (3,4), (2,3)] we will return [(3,4), (2,3)] as the list
    because of the filter function we are passinng, as the value to rove is (1,2) where a=1, b=2 and x=1,y=2 in first value we will get in x from list
    filter will give us false and then that wont be added to the list, hence the end result will be list without tuple (1,2)    
-}

{-
    Now that our function is ready to be used, now we will actually rove the piece from the gameBoard and update the list in the GameState
    We will take in the game state, change the list in it and return back the gamestate, before that we need a function which will give us the middle of the diagonal values
    we are gonna jump to, suppose we have a red at (2,4) and black at (3,3), so we will move the red piece to empty space which is at (4,2), so we are making a function called
    midpoint, which will find the value (3,3) which is the middle of that jump diagonal
-}

middle :: Coord -> Coord -> Coord
middle (x,y) (a,b) = (((x+a) `div` 2), ((y+b) `div` 2)) 

pieceRemover :: GameState -> Coord -> Coord -> GameState
pieceRemover s (x,y) (a,b) 
    | status s == BlackPlayer = s{blackPieces = rover (x,y) (blackPieces s), 
                                  blackKings = rover  (x,y) (blackKings s),
                                  redPieces = rover (middle (x,y) (a,b)) (redPieces s), 
                                  redKings = rover (middle (x,y) (a,b)) (redKings s)}
    | status s == RedPlayer = s{redPieces = rover (x,y) (redPieces s),
                                 redKings = rover  (x,y) (redKings s),
                                 blackPieces = rover (middle (x,y) (a,b)) (blackPieces s),
                                 blackKings = rover (middle (x,y) (a,b)) (blackKings s)}
    | otherwise = s
    
{-
    Now the main thing starts, when we will code the moves for pawns and kings, and we will make 4 functions to achieve this goal of ours, 
    First we shall only implement the simple moves on all the kings and pawns, after that we will look for the jump moves for both of them
-}
{-
    First making the moves for the pawns for both red and black as its the simplest with less conditions... kings will be a headache
    MAKING ALL THE POSSIBLE MOVES FOR THE PAWNS
-}

{-
    We are doing the red pawn below and red pawn can only move up, so...
    Listing all the moves available at the time of game executed
-}

redPawnMoves :: GameState -> PieceState -> [Move] -> [Move]
redPawnMoves s [] m = m
redPawnMoves s ((x,y) : xs) m 
    | isUpLeftVacant s (x,y) && isUpRightVacant s (x,y) = redPawnMoves s xs ([P(x, y),P(x-1,y-1)] : [P(x,y),P(x+1,y-1)]:m)  --P because these are pawns
    | isUpRightVacant s (x,y) = redPawnMoves s xs ([P(x,y), P(x+1,y-1)]:m)         
    | isUpLeftVacant s (x,y) = redPawnMoves s xs ([P(x,y),P(x-1,y-1)]:m)
    | otherwise = redPawnMoves s xs m

{-
    Doing similar case for the black pawns
-}

blackPawnMoves :: GameState -> PieceState -> [Move] -> [Move]
blackPawnMoves gs [] m = m
blackPawnMoves gs ((x,y):xs) m
    | isDownLeftVacant gs (x,y) && isDownRightVacant gs (x,y) = blackPawnMoves gs xs ([P(x,y), P(x+1,y+1)] : [P(x,y), P(x-1,y+1)] : m)
    | isDownLeftVacant gs (x,y) = blackPawnMoves gs xs ([P(x,y), P(x-1,y+1)]:m)
    | isDownRightVacant gs (x,y) = blackPawnMoves gs xs ([P(x,y), P(x+1,y+1)]:m)
    | otherwise = blackPawnMoves gs xs m


{-
    Checker only takes the king arguments for history checking because, only kings are the type of game piece which can go up and down and can actually play the same move again, so this function will
    check that the king is not playing the same move again.
-}
checkerrr:: Move -> [Move] -> Bool
checkerrr _ [] = False      --does not exist in the history
checkerrr _ [a] = False 
checkerrr a ([P(a1,b1), P(a2,b2)] : xs) = False
checkerrr a ([K(a1,b1), P(a2,b2)] : xs) = False
checkerrr a ([P(a1,b1), K(a2,b2)] : xs) = False

checkerrr [K(a1,b1) , K(a2,b2)] ([K(x1,y1), K(x2,y2)] : xsys) | ((a1,b1) == (x1,y1)) && (a2,b2) == (x2,y2) || ((a1,b1) == (x2,y2) && (a2,b2) == (x1,y1)) = True
                                                              | otherwise = checkerrr [K(a1,b1) , K(a2,b2)] xsys




{-   I ran out of time to implement this and try it, but i had such kind of design in mind for repeated states when
 the user wants to enter the next state, so, we will pass the three possible earlier played states (taken from history) and check if they are equal then it will return True, that this is a repeated state
 if false then it means, its not a repeated state and we can go ahead with that move   -}

repeatedState :: Move -> Move -> Move -> GameState -> Bool
repeatedState f s t gs= if (f `elem` history gs && s `elem` history gs && t `elem` history gs) then True else False
{-
    now comes the headache of kings, when for both red and black the king can move up and down in diagonals... so alot more of cases will be here.
    will be crammed up sorry for the inconvenience, but I will try to be as clean as possible
    I will use this function I made below for both red and black kings, cuz they show the same properties/

-}
kingMoves :: GameState -> PieceState -> [Move] -> [Move]
kingMoves gs [] m = m
kingMoves gs ((x,y) : xs) m 
 --Checking all the surrounding diagonals first
    | (isUpLeftVacant gs (x,y)) && (isDownLeftVacant gs (x,y)) && (isUpRightVacant gs (x,y)) && (isDownRightVacant gs (x,y)) 
    && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs)) && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs))
    = kingMoves gs xs ([K(x,y),K(x-1,y-1)] : [K(x,y),K(x-1,y+1)] : [K(x,y),K(x+1,y-1)] : [K(x,y),K(x+1,y+1)] : m)

--Now checking if the surrounding 3 sides are vacant, writing all the possibilities
    | (isUpLeftVacant gs (x,y)) && (isDownLeftVacant gs (x,y)) && (isUpRightVacant gs (x,y))
     && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs))
    = kingMoves gs xs ([K(x,y),K(x-1,y-1)] : [K(x,y),K(x-1,y+1)] : [K(x,y),K(x+1,y-1)] : m)
    
    | (isUpLeftVacant gs (x,y)) && (isDownLeftVacant gs (x,y)) && (isDownRightVacant gs (x,y)) 
    && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs))
    = kingMoves gs xs ([K(x,y),K(x-1,y-1)] : [K(x,y),K(x-1,y+1)] : [K(x,y),K(x+1,y+1)] : m)

    | (isDownRightVacant gs (x,y)) && (isDownLeftVacant gs (x,y)) && (isUpRightVacant gs (x,y)) 
    && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs)) && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs))
    = kingMoves gs xs ([K(x,y),K(x+1,y+1)] : [K(x,y),K(x-1,y+1)] : [K(x,y),K(x+1,y-1)] : m)

    | (isUpLeftVacant gs (x,y)) && (isUpRightVacant gs (x,y)) && (isDownRightVacant gs (x,y)) 
    && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs))
    = kingMoves gs xs ([K(x,y),K(x-1,y-1)] : [K(x,y),K(x+1,y-1)] : [K(x,y),K(x+1,y+1)] : m)

--Now checking if the surrounding 2 diagonals are vacant, writing all the possibilites
    | (isUpLeftVacant gs (x,y)) && (isUpRightVacant gs (x,y)) && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x-1,y-1)] : [K(x,y), K(x+1, y-1)] : m) 
    | (isDownLeftVacant gs (x,y)) && (isDownRightVacant gs (x,y)) && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x-1,y+1)] : [K(x,y), K(x+1, y+1)] : m)
    | (isUpLeftVacant gs (x,y)) && (isDownLeftVacant gs (x,y))  && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x-1,y-1)] : [K(x,y), K(x-1, y+1)] : m)
    | (isUpRightVacant gs (x,y)) && (isDownRightVacant gs (x,y)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x+1,y-1)] : [K(x,y), K(x+1, y+1)] : m)
    | (isUpLeftVacant gs (x,y)) && (isDownRightVacant gs (x,y)) && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs))  = kingMoves gs xs ([K(x,y), K(x-1,y-1)] : [K(x,y), K(x+1, y+1)] : m)
    | (isUpRightVacant gs (x,y)) && (isDownLeftVacant gs (x,y)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs)) && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x+1,y-1)] : [K(x,y), K(x-1, y+1)] : m)
--Now checking if the surrounding 1 diagonal is vacant, writing all the possibilities
    | (isDownLeftVacant gs (x,y))  && not(checkerrr [K(x,y),K(x-1,y+1)] (history gs))  = kingMoves gs xs ([K(x,y), K(x-1,y+1)] : m)
    | (isDownRightVacant gs (x,y)) && not(checkerrr [K(x,y),K(x+1,y+1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x+1,y+1)] : m)
    | (isUpLeftVacant gs (x,y))  && not(checkerrr [K(x,y),K(x-1,y-1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x-1,y-1)] : m)
    | (isUpRightVacant gs (x,y)) && not(checkerrr [K(x,y),K(x+1,y-1)] (history gs)) = kingMoves gs xs ([K(x,y), K(x+1,y-1)] : m)
    | otherwise = kingMoves gs xs m


{-
    Now as the simple moves are done above for both red and black, we will do the similar thing now for the red and black pawns but jumps.
    first doing jumps for red pawns, then jumps for black pawns, they differ as red move from bottom up and black from top down
    But for kings it doenst matter, so we will make only one function which will accomodate for that
    so when a jump is made by a certain piece then the other color piece should be roved from the diagonal too, so we will account for that too in this
-}

redPawnJumps :: GameState -> Coord -> Move -> [Move] 
redPawnJumps gs (x,y) r
    | (isJumpUpLeftVacant gs (x,y)) && (isJumpUpRightVacant gs (x,y)) && (y-2 == 0)         --so this is the condition when red changes to king so the program will go to function kingMoves
    = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r ++ [P(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r ++ [P(x,y)])
    | (isJumpUpLeftVacant gs (x,y)) && (isJumpUpRightVacant gs (x,y)) = redPawnJumps (pieceRemover gs (x,y) (x-2, y-2)) (x-2,y-2) (r++[P(x,y)]) ++ redPawnJumps (pieceRemover gs (x,y) (x+2, y-2)) (x+2,y-2) (r++[P(x,y)])
    | (isJumpUpRightVacant gs (x,y)) && (y-2 == 0) = kingJumps (pieceRemover gs (x,y) (x+2, y-2)) (x+2,y-2) (r ++ [P(x,y)])
    | (isJumpUpRightVacant gs (x,y)) = redPawnJumps (pieceRemover gs (x,y) (x+2, y-2)) (x+2,y-2) (r++[P(x,y)])
    | (isJumpUpLeftVacant gs (x,y)) && (y-2 == 0) = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[P(x,y)])
    | (isJumpUpLeftVacant gs (x,y)) = redPawnJumps (pieceRemover gs (x,y) (x-2, y-2)) (x-2,y-2) (r ++ [P(x,y)])
    | otherwise = [r ++ [P(x,y)]]


{-
    Same thing for the black but different conditions as they move top to down
-}

blackPawnJumps :: GameState -> Coord -> Move -> [Move]
blackPawnJumps gs (x,y) r
     | (isJumpDownLeftVacant gs (x,y)) && (isJumpDownRightVacant gs (x,y)) && (y+2 == 7) = (kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[P(x,y)]))++ (kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[P(x,y)]))
     | (isJumpDownLeftVacant gs (x,y)) && (isJumpDownRightVacant gs (x,y))= (blackPawnJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[P(x,y)]))++ (blackPawnJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[P(x,y)]))
     | (isJumpDownRightVacant gs (x,y)) && (y+2 ==7) = kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[P(x,y)]) 
     | (isJumpDownRightVacant gs (x,y)) = blackPawnJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[P(x,y)]) 
     | (isJumpDownLeftVacant gs (x,y)) && (y+2 == 7) = kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[P(x,y)]) 
     | (isJumpDownLeftVacant gs (x,y)) = blackPawnJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[P(x,y)]) 
     |otherwise = [r ++ [P(x,y)]]


allRedPawnJumps :: GameState -> PieceState -> [Move] -> [Move]          --PieceState contains the coordinates of all the red pawns at this moment
allRedPawnJumps gs [] r = r
allRedPawnJumps gs ((x,y):xs) r | redPawnJumps gs (x,y) [] == [[P(x,y)]] = allRedPawnJumps gs xs r
                                | otherwise = allRedPawnJumps gs xs r ++ redPawnJumps gs (x,y) []

allBlackPawnJumps :: GameState -> PieceState -> [Move] -> [Move]
allBlackPawnJumps gs [] r = r
allBlackPawnJumps gs ((x,y):xs) r | blackPawnJumps gs (x,y) [] == [[P(x,y)]] = allBlackPawnJumps gs xs r
                                  | otherwise = allBlackPawnJumps gs xs r ++ blackPawnJumps gs (x,y) []


kingJumps :: GameState -> Coord -> Move -> [Move]
kingJumps gs (x,y) r 
 | isJumpUpLeftVacant gs (x,y) && isJumpUpRightVacant gs (x,y) && isJumpDownRightVacant gs (x,y) && isJumpDownLeftVacant gs (x,y)
  = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | isJumpUpLeftVacant gs (x,y) && isJumpUpRightVacant gs (x,y) && isJumpDownRightVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)])
 | isJumpUpLeftVacant gs (x,y) && isJumpUpRightVacant gs (x,y) && isJumpDownLeftVacant gs (x,y)= kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | isJumpUpLeftVacant gs (x,y) && isJumpUpRightVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)]) 
 | isJumpUpLeftVacant gs (x,y) && isJumpDownRightVacant gs (x,y) && isJumpDownLeftVacant gs (x,y)= kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | isJumpUpLeftVacant gs (x,y) && isJumpDownRightVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)])
 | isJumpUpLeftVacant gs (x,y) && isJumpDownLeftVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | isJumpUpLeftVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x-2,y-2)) (x-2,y-2) (r++[K(x,y)]) 
 | isJumpUpRightVacant gs (x,y) && isJumpDownRightVacant gs (x,y) && isJumpDownLeftVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | isJumpUpRightVacant gs (x,y) && isJumpDownRightVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)])
 | isJumpUpRightVacant gs (x,y) && isJumpDownLeftVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | isJumpUpRightVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x+2,y-2)) (x+2,y-2) (r++[K(x,y)])
 | isJumpDownRightVacant gs (x,y) && isJumpDownLeftVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)]) ++ kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | isJumpDownRightVacant gs (x,y) = kingJumps (pieceRemover gs (x,y) (x+2,y+2)) (x+2,y+2) (r++[K(x,y)])
 | isJumpDownLeftVacant gs (x,y)  = kingJumps (pieceRemover gs (x,y) (x-2,y+2)) (x-2,y+2) (r++[K(x,y)])
 | otherwise = [r ++ [K(x,y)]]

 {-
    Collecting all the kings moves, and making them into a simpler function
 -}

kingCollectedJumps :: GameState -> [Coord] -> [Move] -> [Move]
kingCollectedJumps gs [] r = r
kingCollectedJumps gs ((x,y) : xs) r 
    | kingJumps gs (x,y) [] == [[K(x,y)]] = kingCollectedJumps gs xs r
    | otherwise = kingCollectedJumps gs xs r ++ kingJumps gs (x,y) []

