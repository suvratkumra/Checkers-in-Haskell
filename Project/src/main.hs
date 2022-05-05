module Main where

import Checkers.Types
--------------------------------------------------------------------------------------
--                     Imports you will need to develop
--------------------------------------------------------------------------------------
-- -- import Checkers.Moves
-- import Checkers.Moves
-- -- import Checkers.ApplyMove
-- import Checkers.ApplyMove
-- import AI.ABsearch


import Checkers.SuvratKumra(moves, apply_move, red_ai, black_ai)
--------------------------------------------------------------------------------------
--                     Standard library imports:
--------------------------------------------------------------------------------------
-- 'System.IO' is needed to set the line buffering (i.e., so we can press backspace
--  in GHCi to do what we expect it should do)
import System.IO
    ( stdin, hSetBuffering, BufferMode(LineBuffering) )
-- 'Text.Read' is needed for 'readMaybe' since 'read' throws an exception by default
--  when it parses, but we want to handle the exception ourselves with the 'Maybe' type.
import Text.Read ( readMaybe )


main :: IO ()
-- main = do -- the Human on Human game configuration
--      hSetBuffering stdin LineBuffering
--      runGame $ GameConfig { movemaker = apply_move
--                           , blackMove = Human
--                           , redMove = Human
--                           , state = initialGameState}

-- -- a stub for apply_move
-- apply_move:: Move -> GameState -> GameState
-- apply_move _ g = g

--   Example of configurations for games:
    
main = do hSetBuffering stdin LineBuffering
          runGame $ GameConfig {movemaker = apply_move
                             , blackMove = AI black_ai
                             , redMove = AI red_ai
                             , state = initialGameState}


-- main = do hSetBuffering stdin LineBuffering
--           runGame $ GameConfig {movemaker = apply_move
--                              , blackMove = Human
--                              , redMove = AI red_ai
--                              , state = initialGameState}


-- main = do hSetBuffering stdin LineBuffering
--           runGame $ GameConfig {movemaker = apply_move
--                              , blackMove = AI (black_ai)
--                              , redMove = Human
--                              , state = initialGameState}



--  Running a game ...

runGame :: GameConfig -> IO ()
runGame g = case (currentMove g) of
    Nothing -> do  -- gameover
        print (state g)
    Just Human -> do  -- Human player
        print (state g)
        print "Enter move as a list of coordinates."
        -- The loop to keep asking for input until the user inputs a valid move.
        let loop = fmap readMaybe getLine >>= \ma -> case ma of
                   Just mv -> return mv
                   Nothing -> putStrLn "Please try again..." >> loop
        move <- loop
        
        let s' = (movemaker g) move (state g)
        runGame $ g{state = s'}
    Just (AI ai_move) -> do -- AI player
        print (state g)
        runGame $ g{state = (movemaker g) (ai_move (state g)) (state g)}
  where 
    currentMove :: GameConfig -> Maybe PlayerType
    currentMove conf = case status (state conf) of
      RedPlayer -> Just (redMove conf)
      BlackPlayer -> Just (blackMove conf)
      GameOver -> Nothing

--Some test states ...

test1 = GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(6,7)]
                  , redKings = [(2,3),(3,2)]
                  , status = RedPlayer
                  , message = "" 
                  , history =[[K (7,6),K (6,7)],[K (1,2),K (2,3)],[K (6,7),K (7,6)],[K (2,1),K (1,2)],[K (7,6),K (6,7)],[K (2,3),K (3,2)],[K (6,7),K (7,6)]] }

test2 = GameState { blackPieces = []
                  , redPieces = []
                  , blackKings = [(6,3),(4,3)]
                  , redKings = [(0,1)]
                  , status = RedPlayer
                  , message = "" 
                  , history = []}

test3 =  GameState { blackPieces = [(2,1),(4,1),(2,3),(4,3)]
                  , redPieces = [(3,4)]
                  , blackKings = [(4,5)]
                  , redKings = [(0,1),(1,0)]
                  , status = RedPlayer
                  , message = "" 
                  , history = []}

test7=  GameState { blackPieces = [(4,3),(6,3),(4,5),(6,5)]
                  , redPieces = [(2,5),(7,0)]
                  , blackKings = [(3,0)]
                  , redKings = [(5,6)]
                  , status = BlackPlayer
                  , message = "" 
                  , history = []}


test4 =  GameState { blackPieces = [(2,1),(4,1),(2,3),(4,3)]
                  , redPieces = [(3,4)]
                  , blackKings = [(4,5)]
                  , redKings = [(1,0),(0,1)]
                  , status = RedPlayer
                  , message = "" 
                  , history = []}


test6 =  GameState { blackPieces = [(7,0), (2,5)]
                  , redPieces = [(4,1),(6,1),(4,3),(6,3),(4,5)]
                  , blackKings = [(5,4)]
                  , redKings = [(3,0)]
                  , status = BlackPlayer
                  , message = "" 
                  , history = []}

test5 =  GameState { blackPieces = []
            , redPieces = []
            , blackKings = [(6,7)]
            , redKings = [(2,1),(3,2)]
            , status = RedPlayer 
            , message = ""
            , history = [[K (7,6),K (6,7)],[K (1,2),K (2,3)],[K (6,7),K (7,6)],[K (2,1),K (1,2)],[K (7,6),K (6,7)],[K (2,3),K (3,2)],[K (6,7),K (7,6)]]}


