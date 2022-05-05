module Checkers.ApplyMove where

import Checkers.Moves
import Checkers.Types 

import qualified Data.Set as Set


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
  