module Checkers.SuvratKumra where
    
import Checkers.Types

----------------------AI-------------------------

{-
The help taken for the code is from a video on Youtube of minmax trees applied in chess.
all the formulas are based on that video. https://www.youtube.com/watch?v=l-hh51ncgDI
-}

top :: Integer
bot :: Integer
ply :: Integer
top = 30000
bot = -30000
ply = 4

redHeuristic::GameState -> Integer
redHeuristic s
 | null (blackPieces s) && null (blackKings s)
  = top
 | null (redPieces s) && null (redKings s)
  = bot
 |otherwise = formula (redPieces s)  0 - formula (blackPieces s)  0 +  2*(formula (redKings s)  0 -
               formula (blackKings s) 0)

blackHeuristic::GameState -> Integer
blackHeuristic s 
 | null (blackPieces s) && null (blackKings s)
 = bot
 | null (redPieces s) &&  null (redKings s)
  = top
 | otherwise =  formula (blackPieces s) 0 - formula (redPieces s)  0 +  2*(formula (blackKings s)  0 -
                formula (redKings s)  0)


formula::[Coord]->Integer-> Integer
formula (x:xs)  count = formula xs  count+1
formula [] count = count

red_ai::GameState -> Move 
red_ai s = moveFinder s (minmax s ply) (remover (moves s))

black_ai::GameState -> Move
black_ai s = moveFinder s (minmax s ply) (remover (moves s))


moveFinder::GameState->Integer->[Move]->Move
moveFinder s h [x] = x  
moveFinder s h (x:xs)
 | h == redHeuristic (apply_move x s)
  = x
 | h == blackHeuristic (apply_move x s)
  = x
 | otherwise = moveFinder s h xs



minmax::GameState -> Integer ->  Integer 
minmax s depth 
 | (depth == 0 && status s == RedPlayer) || status s == GameOver
  = redHeuristic s
 | (depth == 0 && status s == BlackPlayer ) || status s == GameOver
 = blackHeuristic s
 | status s == RedPlayer
  = maxEval s (remover (moves s)) depth bot
 | status s  == BlackPlayer
  = minEval s (remover (moves s)) depth top 
 
maxEval::GameState ->[Move]-> Integer  -> Integer  -> Integer 
maxEval s [] d value = value
maxEval s (x:xs) d value = max (maxEval s xs d value) (max value eval) 
                      where eval = minmax (apply_move x s) (d-1)



minEval:: GameState -> [Move]->Integer -> Integer  -> Integer 
minEval s [] d val = val
minEval s (x:xs)  d val = min (minEval s xs d val) (min val eval)
                      where eval = minmax (apply_move x s) (d-1)


-------------------MOVES---------------

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






















-----------APPLY MOVES------

{-
    Useful function for us to seperate P and K as we only are bothered with using the coordinates of the move
-}
seperater :: Move -> PieceState
seperater [] = [] 
seperater mv = case mv of
    P(x,y) : ps -> (x,y) : seperater ps
    K(x,y) : ks -> (x,y) : seperater ks

{-
    the main function which will take in the possibilities of jump moves, normal moves, first there are total of 9 cases here, 4 are when the status is RedPlayer and 4 for BlackPlayer and 1 where nothing happens
    just initial gamestate is shown first condition for both, checks if the move entered by the user is a pawn move or invalid but there is a jump move available, so handling that condition, telling the user that
    they need to play the jump move before they can play something else. 
    Second case in both checks that, the move is simple only, there are no jump moves available, and that condition is applied, 
    Third case in both is that when the jump move is available, so that will function  requiredly
    and the last case tells us that the move is invalid and the user needs to enter the coordinate again (done in main.).
-}
apply_move :: Move -> GameState -> GameState
apply_move mv gs 
    | status gs == RedPlayer && not(isValid gs mv) && available (jumps gs) = gs{message = "Jump move detected for Red Player, please play that first"}
    | status gs == RedPlayer && isValid gs mv && (mv `elem` simple_moves gs) 
     = if head (seperater mv) `elem` redPieces gs then changeStatus (applyingSimple gs mv) else changeStatus (applyKingSimple gs mv)
    | status gs == RedPlayer && isValid gs mv && (mv `elem` jumps gs) = if (head (seperater mv)) `elem` (redPieces gs) || (head (seperater mv)) `elem` (redKings gs)
                                                                        then changeStatus (applyJumpMoves mv gs) else changeStatus (applyJumpMoves mv gs)
    | status gs == RedPlayer = gs{message = "Move is not valid"}

    | status gs == BlackPlayer && not(isValid gs mv) && available (jumps gs) = gs{message = "Jump move detected for Black Player, please play that first"}
    | status gs == BlackPlayer && isValid gs mv && (mv `elem` simple_moves gs) 
     = if head (seperater mv) `elem` blackPieces gs then changeStatus (applyingSimple gs mv) else changeStatus (applyKingSimple gs mv)
    | status gs == BlackPlayer && isValid gs mv && (mv `elem` jumps gs) = if (head (seperater mv)) `elem` (blackPieces gs) || (head (seperater mv)) `elem` (blackKings gs)
                                                                        then changeStatus (applyJumpMoves mv gs) else changeStatus (applyJumpMoves mv gs)
    | status gs == BlackPlayer = gs{message = "Move is not valid"}
    | otherwise = initialGameState 



{-
    A function to change the status of the game
-}

changeStatus :: GameState -> GameState
changeStatus gs | ((not(available (redPieces gs))) && (not(available (redKings gs)))) || (not(available(remover (moves gs)))) = gs{status = GameOver,            -- I wanted to set this to gameover, but test cases are not passing that way
                                                                                                                                        message = "Gameover"}
                | ((not(available (blackPieces gs))) && (not(available (blackKings gs)))) || (not(available(remover(moves gs)))) = gs{status = GameOver,
                                                                                                                                        message = "Gameover"}
                | otherwise = gs

{-
    seperating the SM or JM from the move so that we can access the PorK[Coord] for our applymoves
-}

remover :: SMorJM [Move] -> [Move]
remover mv = case mv of
    SM(x:xs) -> (x:xs)
    JM(x:xs) -> (x:xs)
    _ -> []


{-
    Checker only takes the king arguments for history checking because, only kings are the type of game piece which can go up and down and can actually play the same move again, so this function will
    check that the king is not playing the same move again.
-}
checking::[Move]->[Move]->Bool 
checking _ [] = False            --When the history is empty, so simply store this in the history
checking [[K(a1,b1), K(a2,b2)]] ([K(x1,y1), K(x2,y2)] : xsys) = if ((a1,b1) == (x1,y1) && (a2,b2) == (x2,y2)) then True 
                                                              else checking [[K(a1,b1), K(a2,b2)]] xsys

{-
    This is a simple function which will tell us if the move is valid or not
-}
isValid :: GameState -> Move  -> Bool
isValid gs [] = False
isValid gs m | m `notElem` remover (moves gs) = False           --I needed to  make that function so that i can compare easily.
             | otherwise = True

{-
    Now we will make two functions which will use the Moves.hs we made and will first make the function which will
    apply thie simple moves to the Redplayer and then the BlackPlayer. before that we will make a list of tuples which contains
    the first row and the last row of the board, this will be helpful in keeping track when the pawn will be converted to a king
-}
firstRow = [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0)]
lastRow = [(0,7), (1,7), (2,7), (3,7), (4,7), (5,7), (6,7), (7,7)]


{-
 applying the simple moves for the pawns, where the redPawn can either move in up left and up right position, depending upon the situation and
 where the black Pawn can move in down left and down right depending upon the condiiton , and then updating the locaiton of the pawns as they moved
 from one to another position, when the pawn is at second row or second last row, it will have to change into the king (black and red respectively) so taking that condition in mind
 so we have 4 cases.
-}
applyingSimple :: GameState -> Move -> GameState
applyingSimple gs corr 
    | status gs == RedPlayer = if head(tail (seperater corr)) `elem` firstRow then
        gs{ message = "Red Player's Turn as Pawn changed into a King",
           status = RedPlayer ,                                         -- This is because in the calgary version of the game we want that when a peice turns into king, teh same color user should still have the move
           redPieces = rover (head (seperater corr)) (redPieces gs) ,
           redKings = head(tail (seperater corr)) : redKings gs,
           history = corr : history gs}
    else 
        gs{ message = "Black Player's Turn", 
           status = BlackPlayer ,
           redPieces = head(tail (seperater corr)) : rover (head (seperater corr)) (redPieces gs),
            history = corr : history gs}

    | status gs == BlackPlayer = if head(tail (seperater corr)) `elem` lastRow then
        gs{ message = "Black Player's Turn as Pawn changed into a King", 
            status = BlackPlayer, 
            blackPieces = rover (head (seperater corr)) (blackPieces gs),
            blackKings = head(tail (seperater corr)) : blackKings gs,
            history = corr : history gs} 
    else
        gs{message = "Red Player's Turn", 
           status = RedPlayer,
           blackPieces = head(tail (seperater corr)) : rover (head (seperater corr)) (blackPieces gs),
            history = corr : history gs }


{-
 applying the simple moves for the king, where the king can either move in any of the four direction, depending upon the situation and then the conditions which will change for the king, as we move its location
 from one to another position, simple function yet took time. 
-}
applyKingSimple :: GameState -> Move -> GameState
applyKingSimple gs corr 
    | status gs == RedPlayer = gs{message = "Black Player's Turn",
                                  status = BlackPlayer,
                                  redKings = head (tail (seperater corr)) : rover (head (seperater corr)) (redKings gs),
                                  history = corr : history gs }
    | status gs == BlackPlayer = gs{message = "Red Player's Turn",
                                    status = RedPlayer,
                                    blackKings = head(tail (seperater corr)) : rover (head (seperater corr)) (blackKings gs),
                                    history = corr : history gs}


{-
    I don't even remember how many hours I spent figuring out this applyJumpMoves function...
    I am so proud of myself that I figured it out at the end, but here it is, super complicated yet working code
    it passes all the test cases listed on gradescope for now, but you guys add more later so lets see what I get overall after the result is out.
    INline commenting for what the function is doing
-}


applyJumpMoves :: Move  -> GameState -> GameState
applyJumpMoves [] gs = gs
applyJumpMoves mv gs = case mv of 
    -- Taking the case when there will be just one coordinate for Pawn or King will be left in the list entered by the user, so it will just return the gameState 
    [P(x,y)] -> gs 
    [K(x,y)] -> gs

    --Now taking the case when the Pawn is at the second last (for Black) and second (for Red) row, and the jump will take it to the position where the pawn will be changed to a king
    --so taking that possibiliy here, notice that the user enters just two coordinates
    [P (x1,y1), K(x2,y2)] -> if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs) && ((x2,y2) `elem` firstRow)
                                then applyJumpMoves [K(x2,y2)] $  gs{message = "Black Player's Turn" ,
                                                                        status = BlackPlayer  ,                --this will not change as per the rules of this checkers we are coding
                                                                        blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs) ,   --removing the blackKing(if available) as it will be in the position of the jump move player is about to make
                                                                        blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs) , --removing the blackPiece(if available) as it will be in the position of the jump move player is about to make
                                                                        redPieces = rover (x1,y1) (redPieces gs),    -- as we are moving the redPiece to a king, so removing it from the location it was stored at before jumping
                                                                        redKings  = (x2,y2):rover (x1,y1) (redKings gs),
                                                                        history = [head (history gs ++ [[P(x1,y1),K(x2,y2)]])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)

                                else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs) && ((x2,y2) `elem` lastRow)
                                then applyJumpMoves [K(x2,y2)] $ gs{message = "Red Player's Turn" ,
                                                                    status =  RedPlayer ,
                                                                    redKings  =  rover (middle (x1,y1)(x2,y2)) (redKings gs) , --removing the redKing(if available) as it will be in the position of the jump move player is about to make
                                                                    redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs), --removing the redPiece(if available) as it will be in the position of the jump move player is about to make
                                                                    blackPieces =  rover (x1,y1) (blackPieces gs) , -- as we are moving the blackPiece to a king, so removing it from the location it was stored at before jumping
                                                                    blackKings = (x2,y2):rover (x1,y1) (blackKings gs),
                                                                    history = [head (history gs ++ [[P(x1,y1),K(x2,y2)]])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                                else applyJumpMoves [] gs

    --Now taking the case when the Pawn will just simply jump from one pawn location to another while cutting the opposite color Pawn or King, (not converting to king, ie, last row or first row depending on the color)
    --so taking that possibiliy here, notice that the user enters just two coordinates
    [P (x1,y1), P(x2,y2)] -> if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs)
                      then applyJumpMoves [P(x2,y2)] $ gs{message = "Black Player's turn" ,
                                                                status = BlackPlayer,
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs), --removing the blackKing(if available) as it will be in the position of the jump move player is about to make
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs), --removing the blackPiece(if available) as it will be in the position of the jump move player is about to make
                                                                redPieces = (x2,y2):rover (x1,y1) (redPieces gs), -- as we are moving the redPiece, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                                history = [head (history gs ++ [[P(x1,y1),P(x2,y2)]])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                        else if(status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs)
                            then applyJumpMoves [P(x2,y2)] $ gs {message = "Red Player's turn",
                                                                status = RedPlayer,
                                                                redPieces = rover (middle (x1,y1) (x2,y2)) (redPieces gs), --removing the redPiece(if available) as it will be in the position of the jump move player is about to make
                                                                redKings = rover (middle (x1,y1) (x2,y2)) (redKings gs), --removing the redKing(if available) as it will be in the position of the jump move player is about to make
                                                                blackPieces = (x2,y2) : rover (x1,y1) (blackPieces gs), -- as we are moving the blackPiece, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                                history = [head (history gs ++ [[P(x1,y1),P(x2,y2)]])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                      else applyJumpMoves [] gs

    --Now taking the case when the King will just simply jump from one King location to another while cutting the opposite color Pawn or King
    --so taking that possibiliy here, notice that the user enters just two coordinates
    [K(x1,y1), K(x2,y2)]  -> if (status gs == RedPlayer) && ((x1,y1) `elem` redKings gs)
                        then applyJumpMoves [K(x2,y2)] $ gs{message = "Black Player's  turn" ,
                                                                status = BlackPlayer , 
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs), --removing the blackKing(if available) as it will be in the position of the jump move player is about to make
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs), --removing the blackPiece(if available) as it will be in the position of the jump move player is about to make
                                                                redKings  = (x2,y2):rover (x1,y1) (redKings gs), -- as we are moving the redKing, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                                history = [head (history gs ++ [[K(x1,y1),K(x2,y2)]])] }--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)

                      else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackKings gs)
                        then applyJumpMoves [K(x2,y2)] $ gs{message = "Red Player's  turn" , 
                                                            status = RedPlayer ,
                                                            redKings  = rover (middle (x1,y1)(x2,y2)) (redKings gs), --removing the redKing(if available) as it will be in the position of the jump move player is about to make
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs) , --removing the redPiece(if available) as it will be in the position of the jump move player is about to make
                                                            blackKings = (x2,y2):rover (x1,y1) (blackKings gs), -- as we are moving the blackKing, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                            history = [head (history gs ++ [[K(x1,y1),K(x2,y2)]])] }--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                      else 
                          applyJumpMoves [] gs

    --Now considering the case when we have a list of moves entered by the user which either starts from Pawn jumping to be a king or it is found in the middle of the list (while after these 2 moves the list has a tail)
    -- it will just simply do what it did above for the condition of P -> K,
    P (x1,y1) : K(x2,y2) : xsys  -> if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs) && ((x2,y2) `elem` firstRow)
                                then applyJumpMoves (K(x2,y2):xsys) $  gs{message = "Red Player's Turn" ,
                                                                        status = RedPlayer  ,                --this will not change as per the rules of this checkers we are coding
                                                                        blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs) , --removing the blackKing(if available) as it will be in the position of the jump move player is about to make
                                                                        blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs) , --removing the blackPiece(if available) as it will be in the position of the jump move player is about to make
                                                                        redPieces = rover (x1,y1) (redPieces gs),      -- as we are moving the redPiece, so removing it from the location it was stored at before jumping
                                                                        redKings  = (x2,y2):rover (x1,y1) (redKings gs), -- as we are moving the redKing, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                                        history = [head (history gs ++ [P(x1,y1):K(x2,y2):xsys])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)

                                else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs) && ((x2,y2) `elem` lastRow)
                                then applyJumpMoves (K(x2,y2):xsys) $ gs{message = "Black Player's Turn" ,
                                                                    status =  BlackPlayer ,
                                                                    redKings  =  rover (middle (x1,y1)(x2,y2)) (redKings gs) ,  --removing the redKing(if available) as it will be in the position of the jump move player is about to make
                                                                    redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs), --removing the redPiece(if available) as it will be in the position of the jump move player is about to make
                                                                    blackPieces =  rover (x1,y1) (blackPieces gs) , -- as we are moving the blackPiece, so removing it from the location it was stored at before jumping 
                                                                    blackKings = (x2,y2):rover (x1,y1) (blackKings gs), -- as we are moving the blackKing, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                                    history = [head (history gs ++ [P(x1,y1):K(x2,y2):xsys])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                                else applyJumpMoves [] gs

    --Now considering the case when simple Pawn jump is listed by the user in the moves and it has a list, i.e, even after the two moves the list has a tail, simple pawn jumps
    P (x1,y1) : P(x2,y2) : xsys  -> if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs)
                      then applyJumpMoves (P(x2,y2):xsys) $ gs{message = "Black Player's turn" ,
                                                                status = RedPlayer,
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs), --removing the blackKing(if available) as it will be in the position of the jump move player is about to make
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs), --removing the blackPiece(if available) as it will be in the position of the jump move player is about to make
                                                                redPieces = (x2,y2):rover (x1,y1) (redPieces gs), -- as we are moving the redPiece, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                                history = [head (history gs ++ [P(x1,y1):P(x2,y2):xsys])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                                                            
                            
                      else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs)
                        then applyJumpMoves (P(x2,y2):xsys) $ gs{message = "Red Player's turn" ,
                                                            status = BlackPlayer ,
                                                            redKings  = rover (middle (x1,y1)(x2,y2)) (redKings gs) , --removing the redKing(if available) as it will be in the position of the jump move player is about to make
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs)  , --removing the redPiece(if available) as it will be in the position of the jump move player is about to make
                                                            blackPieces = (x2,y2):rover (x1,y1) (blackPieces gs), -- as we are moving the blackPiece, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                            history = [head (history gs ++ [P(x1,y1):P(x2,y2):xsys])]}--adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                      else applyJumpMoves [] gs

    
    -- Now considering the case when simple King jump is made from one positon to another cutting either an opposite color king or pawn in btw
    K(x1,y1) : K(x2,y2) : xsys  -> if (status gs == RedPlayer) && ((x1,y1) `elem` redKings gs)
                        then applyJumpMoves (K(x2,y2):xsys) $ gs{message = "RedPlayer's turn " ,
                                                                status = RedPlayer , 
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs), --removing the blackKing(if available) as it will be in the position of the jump move player is about to make
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs), --removing the blackPiece(if available) as it will be in the position of the jump move player is about to make
                                                                redKings  = (x2,y2):rover (x1,y1) (redKings gs), -- as we are moving the redKing, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                                history =  [head (history gs ++ [K(x1,y1):K(x2,y2):xsys])] }         --adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)

                      else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackKings gs)
                        then applyJumpMoves (K(x2,y2):xsys) $ gs{message = "BlackPlayer's turn" , 
                                                            status = BlackPlayer ,
                                                            redKings  = rover (middle (x1,y1)(x2,y2)) (redKings gs), --removing the redKing(if available) as it will be in the position of the jump move player is about to make
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs) ,  --removing the redKing(if available) as it will be in the position of the jump move player is about to make
                                                            blackKings = (x2,y2):rover (x1,y1) (blackKings gs), -- as we are moving the blackKing, so removing it from the location it was stored at before jumping and playing to the new place (jumped location)
                                                            history =  [head (history gs ++ [K(x1,y1):K(x2,y2):xsys])] } --adding the whole list once and then later iterations, not allowing it to add and just taking back the biggest list enterd by the user, as it will be the history, (tracing the code will help)
                      else 
                          applyJumpMoves [] gs
                       
    -- this is never supposed to execute, but if it does i have no idea how you did that
    _ -> applyJumpMoves [] $ gs{message = "Hmm, I am bamboozled by this move of yours"}
  