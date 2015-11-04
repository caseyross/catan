-- | Main entry point to the application.
module Main where

import GameState
import Pieces
import Board

import System.Random

-- | The main entry point.
main :: IO ()
main = do
    gen <- getStdGen
    let game = newGame (map newPlayer ["A", "B"]) gen
    let gen' = (snd (next gen))
    runGame game gen'

runGame :: (RandomGen g) => GameState -> g -> IO ()
runGame game gen = do
    let (die1, gen') = rollD6 gen
    let (die2, gen'') = rollD6 gen'
    let game' = updateGame (RollDice die1 die2) game
    print game'
    input <- getLine
    if input == "q"
        then return ()
        else do
            let game'' = updateGame (getAction input) game'
            runGame game'' gen''

rollD6 :: (RandomGen g) => g -> (Int, g)
rollD6 = randomR (1, 6)

getAction :: String -> Action
getAction input
    | null input        = NoAction
    | head input == 'r' = BuildRoad (arg 1) (arg 2)
    | head input == 's' = BuildSettlement (arg 1) (arg 2)
    | head input == 'c' = BuildCity (arg 1) (arg 2)
    | input == "e"      = EndTurn
    | otherwise         = NoAction
    where arg n = read (words input !! n)

updateGame :: Action -> GameState -> GameState

updateGame EndTurn game                         = game { players = tail (players game) }

updateGame (BuildRoad hex edge) game            = game { board =    if canBuyRoad currentPlayer
                                                                        then newBoard
                                                                        else currentBoard
                                                       , players =  if newBoard == currentBoard || not (canBuyRoad currentPlayer)
                                                                        then currentPlayer : tail (players game)
                                                                        else (buyRoad currentPlayer) : tail (players game)}
                                                                    where currentPlayer = head (players game)
                                                                          currentBoard = board game
                                                                          newBoard = placeRoad (hex, edge, currentPlayer) currentBoard

updateGame (BuildSettlement hex corner) game    = game { board  =   if canBuySettlement currentPlayer
                                                                        then newBoard
                                                                        else currentBoard
                                                       , players =  if newBoard == currentBoard || not (canBuySettlement currentPlayer)
                                                                        then currentPlayer : tail (players game)
                                                                        else (buySettlement currentPlayer) : tail (players game)}
                                                                    where currentPlayer = head (players game)
                                                                          currentBoard = board game
                                                                          newBoard = placeSettlement (hex, corner, currentPlayer) currentBoard

updateGame (BuildCity hex corner) game          = game { board =    if canBuyCity currentPlayer
                                                                        then newBoard
                                                                        else currentBoard
                                                       , players =  if newBoard == currentBoard || not (canBuyCity currentPlayer)
                                                                        then currentPlayer : tail (players game)
                                                                        else (buyCity currentPlayer) : tail (players game)}
                                                                    where currentPlayer = head (players game)
                                                                          currentBoard = board game
                                                                          newBoard = placeCity (hex, corner, currentPlayer) currentBoard

updateGame (RollDice die1 die2) game            = game { roll = die1 + die2 }

updateGame NoAction game                        = game

data Action = NoAction
            | RollDice Int Int
            | EndTurn
            | BuildRoad Int Int
            | BuildSettlement Int Int
            | BuildCity Int Int
            | BuyDevCard
            | Trade (Player, Resources) (Player, Resources)
            | TradeBank (Player, Resources)
            deriving Eq