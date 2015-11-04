module Board where

import Pieces

import qualified Data.IntMap as IntMap
import System.Random
import System.Random.Shuffle
import Data.Maybe
import Data.Char

--

type Board = IntMap.IntMap Hex

--

instance Show Hex where
    show h = unlines [line1, line2, line3, line4, line5]
        where line1 =   case fmap status (corner 10) of Just EmptyCorner     -> if fmap port (corner 10) == Just NoPort then "   ." else "   o"
                                                        Just (Settlement pl) -> "   " ++ [(toLower . head) (name pl)]
                                                        Just (City pl)       -> "   " ++ [(toUpper . head) (name pl)]
                        ++ (if roadOnEdge 7 then "___" else "...")
                        ++ case fmap status (corner 11) of Just EmptyCorner      -> if fmap port (corner 11) == Just NoPort then ".   " else "o   "
                                                           Just (Settlement pl)  -> (toLower . head) (name pl) : "   "
                                                           Just (City pl)        -> (toUpper . head) (name pl) : "   "

              line2 =   (if roadOnEdge 3 then "  /" else "  ;")
                        ++ "     "
                        ++ (if roadOnEdge 4 then "\\  " else "`  ")

              line3 =   case fmap status (corner (-1)) of Just EmptyCorner     -> if fmap port (corner (-1)) == Just NoPort then "." else "o"
                                                          Just (Settlement pl) -> [(toLower . head) (name pl)]
                                                          Just (City pl)       -> [(toUpper . head) (name pl)]
                        ++ (if roadOnEdge 3 then "/  " else ";  ")
                        ++ case terrain h of Pasture    -> "~~~"
                                             Plains     -> "==="
                                             Forest     -> "ttt"
                                             Hills      -> "###"
                                             Mountains  -> "^^^"
                                             Desert     -> "   "
                        ++ (if roadOnEdge 4 then "  \\" else "  `")
                        ++ case fmap status (corner 1) of Just EmptyCorner     -> if fmap port (corner 1) == Just NoPort then "." else "o"
                                                          Just (Settlement pl) -> [(toLower . head) (name pl)]
                                                          Just (City pl)       -> [(toUpper . head) (name pl)]

              line4 =   (if roadOnEdge (-4) then " \\  " else " `  ")
                        ++ if robber h then " R " else
                            (if productionNum h < 10 then " " else "")
                            ++ Prelude.show (productionNum h)
                        ++ (if roadOnEdge (-3) then "   / " else "   ; ")

              line5 =   (if roadOnEdge (-4) then "  \\" else "  `")
                        ++ case fmap status (corner (-11)) of Just EmptyCorner     -> if fmap port (corner (-11)) == Just NoPort then "." else "o"
                                                              Just (Settlement pl) -> [(toLower . head) (name pl)]
                                                              Just (City pl)       -> [(toUpper . head) (name pl)]
                        ++ (if roadOnEdge (-7) then "___" else "...")
                        ++ case fmap status (corner (-10)) of Just EmptyCorner     -> if fmap port (corner (-10)) == Just NoPort then "." else "o"
                                                              Just (Settlement pl) -> [(toLower . head) (name pl)]
                                                              Just (City pl)       -> [(toUpper . head) (name pl)]
                        ++ (if roadOnEdge (-3) then "/  " else ";  ")

              roadOnEdge :: Int -> Bool
              roadOnEdge n = isRoad $ IntMap.lookup n (edges h)

              isRoad :: Maybe Edge -> Bool
              isRoad (Just (Road _))    = True
              isRoad _                  = False

              corner :: Int -> Maybe Corner
              corner n = IntMap.lookup n (corners h)

showBoard :: Show a => IntMap.IntMap a -> String
showBoard b = unlines $ foldl combineColumns column1 [column2, column3, column4, column5]
    where column1 = map (unlines . map (take 8) . lines)            (emptyLn 4 ++ showColumn [6, -1, -8] ++ emptyLn 4)
          column2 = map (unlines . map (drop 1 . take 8) . lines)   (emptyLn 2 ++ showColumn [10, 3, -4, -11] ++ emptyLn 2)
          column3 = map (unlines . map (drop 1 . take 10) . lines)  (showColumn [14, 7, 0, -7, -14])
          column4 = map (unlines . map (drop 3 . take 10) . lines)  (emptyLn 2 ++ showColumn [11, 4, -3, -10] ++ emptyLn 2)
          column5 = map (unlines . map (drop 3) . lines)            (emptyLn 4 ++ showColumn [8, 1, -6] ++ emptyLn 4)

          emptyLn n = replicate n "         "

          showColumn :: [Int] -> [String]
          showColumn hexes = squish . map Prelude.show $ catMaybes (zipWith IntMap.lookup hexes (replicate (length hexes) b))

          squish :: [String] -> [String]
          squish s = head s : map (unlines . tail . lines) (tail s)

          combineColumns :: [String] -> [String] -> [String]
          combineColumns a b = zipWith (++) (concatMap lines a) (concatMap lines b)

--

generateBoard :: (RandomGen g) => g -> Board
generateBoard g = hexLayout
    where   hexLayout = IntMap.fromList $ landHexList ++ waterHexList
            landHexList = zip hexIDs shuffledHexes
            waterHexList = zip waterHexIDs (repeat (createHex Water 0))
            shuffledHexes = shuffle' landHexes (length landHexes) $ fst (split g)

            landHexes = createHex Desert 0 : zipWith createHex Pieces.terrains shuffledMarkers
            shuffledMarkers = shuffle' markers (length markers) $ fst (split g)

            createHex :: Terrain -> Int -> Hex
            createHex Water _     = Hex Water 0 False initEdges initCorners
            createHex Desert _    = Hex Desert 0 True initEdges initCorners
            createHex a b         = Hex a b False initEdges initCorners

            initEdges = IntMap.fromList $ zip [-7, -4, -3, 3, 4, 7] (repeat EmptyEdge)
            initCorners = IntMap.fromList $ zip [-11, -10, -1, 1, 10, 11] (repeat (Corner EmptyCorner NoPort))

--

placeRoad :: (Int, Int, Player) -> Board -> Board
placeRoad (hex, edge, pl) = (IntMap.adjust (addRoad (edge, pl)) hex) . (IntMap.adjust (addRoad (0 - edge, pl)) (hex + edge))

addRoad :: (Int, Player) -> Hex -> Hex
addRoad (edge, pl) hex = hex {edges = (IntMap.adjust (\_ -> Road pl) edge (edges hex))}

placeSettlement :: (Int, Int, Player) -> Board -> Board
placeSettlement (hex, corner, pl) = (IntMap.adjust (addSettlement (corner, pl)) hex)
                                    . (IntMap.adjust (addSettlement (corner2, pl)) hex2)
                                    . (IntMap.adjust (addSettlement (corner3, pl)) hex3)
                                    where (corner2, hex2, corner3, hex3) = case corner of (-11) -> (1, hex - 4, 10, hex - 7)
                                                                                          (-10) -> (-1, hex - 3, 11, hex - 7)
                                                                                          (-1)  -> (11, hex - 4, -10, hex + 3)
                                                                                          1     -> (-11, hex + 4, 10, hex - 3)
                                                                                          10    -> (-11, hex + 7, 1, hex + 3)
                                                                                          11    -> (-10, hex + 7, -1, hex + 4)

addSettlement :: (Int, Player) -> Hex -> Hex
addSettlement (corner, pl) hex = hex {corners = (IntMap.adjust (adjustCorner (Settlement pl)) corner (corners hex))}

placeCity :: (Int, Int, Player) -> Board -> Board
placeCity (hex, corner, pl) = (IntMap.adjust (addCity (corner, pl)) hex)
                            . (IntMap.adjust (addCity (corner2, pl)) hex2)
                            . (IntMap.adjust (addCity (corner3, pl)) hex3)
                            where (corner2, hex2, corner3, hex3) = case corner of (-11) -> (1, hex - 4, 10, hex - 7)
                                                                                  (-10) -> (-1, hex - 3, 11, hex - 7)
                                                                                  (-1)  -> (11, hex - 4, -10, hex + 3)
                                                                                  1     -> (-11, hex + 4, 10, hex - 3)
                                                                                  10    -> (-11, hex + 7, 1, hex + 3)
                                                                                  11    -> (-10, hex + 7, -1, hex + 4)

addCity :: (Int, Player) -> Hex -> Hex
addCity (corner, pl) hex = hex {corners = (IntMap.adjust (adjustCorner (City pl)) corner (corners hex))}

adjustCorner :: CornerType -> Corner -> Corner
adjustCorner a (Corner b c) = Corner a c