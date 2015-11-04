module GameState where

import Board
import Pieces

import System.Random

data GameState = GameState  { board   :: Board
                            , players :: [Player]
                            , roll    :: Int
                            }

newGame :: (RandomGen g) => [Player] -> g -> GameState
newGame pl g = GameState (generateBoard g) (cycle pl) 0

instance Show GameState where
    show game = unlines ["It is " ++ playerName ++ "'s turn. " ++ playerName ++ " rolled " ++ show (roll game) ++ "."
                        , ""
                        , "Grain : " ++ show grain
                        , "Wool  : " ++ show wool
                        , "Lumber: " ++ show lumber
                        , "Brick : " ++ show brick
                        , "Ore   : " ++ show ore
                        , ""
                        , showBoard (board game)]
                        where playerName = name (head (players game))
                              (grain, wool, lumber, brick, ore) = hand (head (players game))
