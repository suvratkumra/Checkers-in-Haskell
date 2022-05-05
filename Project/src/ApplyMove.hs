module Checkers.ApplyMove where

import Checkers.Moves
import Checkers.Types 

seperater :: Move -> PieceState
seperater [] = [] 
seperater mv = case mv of
    P(x,y) : ps -> (x,y) : seperater ps
    K(x,y) : ks -> (x,y) : seperater ks


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
changeStatus gs | ((not(available (redPieces gs))) && (not(available (redKings gs)))) || (not(available(movesDiffParameters gs))) = gs{status = GameOver,
                                                                                                                                        message = "Black Player Wins"}
                | ((not(available (blackPieces gs))) && (not(available (blackKings gs)))) || (not(available(movesDiffParameters gs))) = gs{status = GameOver,
                                                                                                                                        message = "Red Player Wins"}
                | otherwise = gs


{-
    Checker only takes the king arguments for history checking because, only kings are the type of game piece which can go up and down and can actually play the same move again, so this function will
    check that the king is not playing the same move again.
-}
checker :: [Move] -> Move -> Move -> Bool
checker [] _ _ = False
checker [a] _ _ = False
checker ([K(x1,y1)] : [K(x2,y2)] : xsys) [K(a1,b1)] [K(a2,b2)] | (x1,y1) == (a1,y1) && (x2,y2) == (a2,y2) = True
                                                               | otherwise = False     

{-
    This is a simple function which will tell us if the move is valid or not
-}
isValid :: GameState -> Move  -> Bool
isValid gs [] = False
isValid gs m | m `notElem` movesDiffParameters gs = False           --I needed to  make that function so that i can compare easily.
             | otherwise = True

{-
    Now we will make two functions which will use the Moves.hs we made and will first make the function which will
    apply thie simple moves to the Redplayer and then the BlackPlayer. before that we will make a list of tuples which contains
    the first row and the last row of the board, this will be helpful in keeping track when the pawn will be converted to a king
-}
firstRow = [(0,0), (1,0), (2,0), (3,0), (4,0), (5,0), (6,0), (7,0)]
lastRow = [(0,7), (1,7), (2,7), (3,7), (4,7), (5,7), (6,7), (7,7)]

{-
    Now implementing the simple move section, using the moves we defined in Moves.hs
-}
{-
    type Move = [PorK Coord];  Coord = (Int, Int)
    rover :: Coord -> PieceState -> PieceState
    type Coord = (Int,Int)
    type PieceState = [Coord]
-}

detecter :: Move -> Int 
detecter mv = case mv of 
    [P(x,y)] -> 1
    [K(x,y)] -> 2


applyingSimple :: GameState -> Move -> GameState
applyingSimple gs corr 
    | status gs == RedPlayer = if head(tail (seperater corr)) `elem` firstRow then
        gs{ message = "Black Player's Turn",
           status = RedPlayer ,                                         -- This is because in the calgary version of the game we want that when a peice turns into king, teh same color user should still have the move
           redPieces = rover (head (seperater corr)) (redPieces gs) ,
           redKings = head(tail (seperater corr)) : redKings gs}
    else 
        gs{ message = "Black Player's Turn", 
           status = BlackPlayer ,
           redPieces = head(tail (seperater corr)) : rover (head (seperater corr)) (redPieces gs)}

    | status gs == BlackPlayer = if head(tail (seperater corr)) `elem` lastRow then
        gs{ message = "Red Player's Turn", 
            status = BlackPlayer, 
            blackPieces = rover (head (seperater corr)) (blackPieces gs),
            blackKings = head(tail (seperater corr)) : blackKings gs} 
    else
        gs{message = "Red Player's Turn", 
           status = RedPlayer,
           blackPieces = head(tail (seperater corr)) : rover (head (seperater corr)) (blackPieces gs) }

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
-}



applyJumpMoves :: Move  -> GameState -> GameState
applyJumpMoves [] gs = gs
applyJumpMoves mv gs = case mv of 
    [P(x,y)] -> gs 
    [K(x,y)] -> gs
    P (x1,y1) : P(x2,y2) : xsys  -> if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs) && notElem(x2,y2) firstRow
                      then applyJumpMoves (P(x2,y2):xsys) $ gs{message = "Black Player's turn" ,
                                                                status = BlackPlayer,
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs),
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs),
                                                                redPieces = (x2,y2):rover (x1,y1) (redPieces gs),
                                                                history = [P(x2,y2)] : history gs }
                      else if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs) && ((x2,y2) `elem` firstRow)
                        then applyJumpMoves (P(x2,y2):xsys) $  gs{message = "Black Player's turn" ,
                                                                status = BlackPlayer ,
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs) ,
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs) ,
                                                                redPieces = rover (x1,y1) (redPieces gs),
                                                                redKings  = (x2,y2):rover (x1,y1) (redKings gs),
                                                                history = [P(x2,y2)] : history gs}
                      else if (status gs == RedPlayer) && ((x1,y1) `elem` redKings gs)
                        then applyJumpMoves (P(x2,y2):xsys) $ gs{message = "Black Player's turn" ,
                                                                status = BlackPlayer , 
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs),
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs),
                                                                redKings  = (x2,y2):rover (x1,y1) (redKings gs),
                                                                history = [P(x2,y2)] : history gs}
                            
                      else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs) && notElem(x2,y2) lastRow
                        then applyJumpMoves (P(x2,y2):xsys) $ gs{message = "Red Player's turn" ,
                                                            status = RedPlayer ,
                                                            redKings  = rover (middle (x1,y1)(x2,y2)) (redKings gs) ,
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs)  ,
                                                            blackPieces = (x2,y2):rover (x1,y1) (blackPieces gs),
                                                            history = [P(x2,y2)] : history gs }

                      else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs) && ((x2,y2) `elem` lastRow)
                        then applyJumpMoves (P(x2,y2):xsys) $ gs{message = "Red Player's turn" ,
                                                            status =  RedPlayer ,
                                                            redKings  =  rover (middle (x1,y1)(x2,y2)) (redKings gs) ,
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs),
                                                            blackPieces =  rover (x1,y1) (blackPieces gs)  ,
                                                            blackKings = (x2,y2):rover (x1,y1) (blackKings gs),
                                                            history = [P(x2,y2)] : history gs }
                      else --if (status gs == BlackPlayer) && ((x1,y1) `elem` blackKings gs)
                         applyJumpMoves (P(x2,y2):xsys) $ gs{message = "Red Player's turn" ,
                                                            status = RedPlayer ,
                                                            redKings  = rover (middle (x1,y1)(x2,y2)) (redKings gs),
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs) ,
                                                            blackKings = (x2,y2):rover (x1,y1) (blackKings gs),
                                                            history = [P(x2,y2)] : history gs }
    K(x1,y1) : K(x2,y2) : xsys  -> if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs) && notElem(x2,y2) firstRow
                      then applyJumpMoves (K(x2,y2):xsys) $ gs{message = "Black Player's KING turn" ,
                                                                status = BlackPlayer,
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs),
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs),
                                                                redPieces = (x2,y2):rover (x1,y1) (redPieces gs),
                                                                history = [K(x1,y1)] : [K(x2,y2)] : history gs }
                      else if (status gs == RedPlayer) && ((x1,y1) `elem` redPieces gs) && ((x2,y2) `elem` firstRow)
                        then applyJumpMoves (K(x2,y2):xsys) $  gs{message = "Black Player's KING turn" ,
                                                                status = BlackPlayer ,
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs) ,
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs) ,
                                                                redPieces = rover (x1,y1) (redPieces gs),
                                                                redKings  = (x2,y2):rover (x1,y1) (redKings gs),
                                                                history = [K(x1,y1)] : [K(x2,y2)] : history gs}
                      else if (status gs == RedPlayer) && ((x1,y1) `elem` redKings gs)
                        then applyJumpMoves (K(x2,y2):xsys) $ gs{message = "Black Player's KING turn" ,
                                                                status = BlackPlayer , 
                                                                blackKings = rover (middle (x1,y1)(x2,y2)) (blackKings gs),
                                                                blackPieces = rover (middle (x1,y1)(x2,y2)) (blackPieces gs),
                                                                redKings  = (x2,y2):rover (x1,y1) (redKings gs),
                                                                history = [K(x1,y1)] : [K(x2,y2)] : history gs}
 
                      else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs) && notElem(x2,y2) lastRow
                        then applyJumpMoves (K(x2,y2):xsys) $ gs{message = "Red Player's KING turn" ,
                                                            status = RedPlayer ,
                                                            redKings  = rover (middle (x1,y1)(x2,y2)) (redKings gs) ,
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs)  ,
                                                            blackPieces = (x2,y2):rover (x1,y1) (blackPieces gs),
                                                            history = [K(x1,y1)] : [K(x2,y2)] : history gs }

                      else if (status gs == BlackPlayer) && ((x1,y1) `elem` blackPieces gs) && ((x2,y2) `elem` lastRow)
                        then applyJumpMoves (K(x2,y2):xsys) $ gs{message = "Red Player's KING turn" ,
                                                            status =  RedPlayer ,
                                                            redKings  =  rover (middle (x1,y1)(x2,y2)) (redKings gs) ,
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs),
                                                            blackPieces =  rover (x1,y1) (blackPieces gs)  ,
                                                            blackKings = (x2,y2):rover (x1,y1) (blackKings gs),
                                                            history = [K(x1,y1)] : [K(x2,y2)] : history gs }
                      else --if (status gs == BlackPlayer) && ((x1,y1) `elem` blackKings gs)
                        applyJumpMoves (K(x2,y2):xsys) $ gs{message = "Red Player's KING turn" ,
                                                            status = RedPlayer ,
                                                            redKings  = rover (middle (x1,y1)(x2,y2)) (redKings gs),
                                                            redPieces = rover (middle (x1,y1)(x2,y2)) (redPieces gs) ,
                                                            blackKings = (x2,y2):rover (x1,y1) (blackKings gs),
                                                            history = [K(x1,y1)] : [K(x2,y2)] : history gs }
                       
    _ -> applyJumpMoves [] gs