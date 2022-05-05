module AI.ABsearch where

import Checkers.ApplyMove
import Checkers.Moves
import Checkers.Types

















---------------------------AI-------------------------------------
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




