module Checkers
    ( main,startGame,continueGameEvE,continueGamePvE,continueGamePvP,showTile,showTilesToMove,showPossibleMoves,test,
    testBoard,turnAI,turnPlayer,minimax,evaluateBoard,isEndGame,emptyBoard,printBoard,initialBoard,initialTile,oppositeSide,
    setTile,getColor,getTile,moveTileGodMode,moveTile,attackTile,enemiesAround,enemiesAroundSeries,afterAttackPos,allMovesFrom,
    allMovesSide,getEnemiesSeries,getEnemiesOfSide,isNewKing,setNewKing,playerDirs,boardToString,onlyOneSide,isInsideBoard
    ) where

import Data.Maybe as Maybe
import Data.List as List
import Data.Ord as Ord

data Color = White | Black deriving (Eq,Show)
data Piece = Pawn | King  deriving (Eq,Show)
data Mode = PvP | PvE | EvE deriving Eq
type Tile = Maybe (Color, Piece)
type Board = [[Tile]] --Synonym for Board
type Pos = (Int,Int) --Position on the board. X=row, Y=column
type TilePos = (Tile,Pos) --Tile and its position on the board

showTile :: Tile -> String
showTile Nothing = "-"
showTile (Just (White,Pawn)) = "w"
showTile (Just (Black,Pawn)) = "b"
showTile (Just (White,King)) = "W"
showTile (Just (Black,King)) = "B"
    
main :: IO() --start program
main = do
    putStrLn "Welcome! Choose mode:"
    putStrLn "PvP is 1"
    putStrLn "PvE is 2"
    putStrLn "Bot vs Bot (EvE) is 3"
    index <- getLine --Read index from user
    case (read index) of
        1 -> startGame PvP
        2 -> startGame PvE
        3 -> startGame EvE
        _ -> main

startGame :: Mode -> IO()
startGame PvE = do
    let start = initialBoard
    putStrLn "Choose difficulty of AI"
    putStrLn "Easy maxDepth = 2 || Normal maxDepth = 3 || Hard maxDepth = 4"
    putStrLn "Enter maxDepth of Minimax: "
    index2 <- getLine
    let maxDepth = read index2
    putStrLn "Choose your color:"
    putStrLn "White is 1"
    putStrLn "Black is 2"
    index <- getLine --Read index from user
    printBoard start
    case read index of  
        1->turnPlayer PvE White ((-1,-1),[]) maxDepth start
        2->turnAI PvE White ((-1,-1),[]) maxDepth start
        _->startGame PvE

startGame PvP = do
    let start = initialBoard
    printBoard start
    turnPlayer PvP White ((-1,-1),[]) 0 start

startGame EvE = do
    let start = initialBoard
    putStrLn "Choose difficulty of AI."
    putStrLn "Easy maxDepth = 2 || Normal maxDepth = 3 || Hard maxDepth = 4"
    putStrLn "Enter maxDepth of Minimax: "
    index <- getLine
    let maxDepth = read index
    printBoard start
    turnAI EvE White ((-1,-1),[]) maxDepth start

continueGamePvE :: Bool -> Bool -> Pos -> Color -> Int -> Board -> IO () --Bool = True if previous move was attack, Pos where piece moved,
continueGamePvE attack fromPlayer currentPos prevSide maxDepth board = do
    let newEnemies = if attack then getEnemiesSeries currentPos board --get [Enemies] list of enemies at new position, that can be attacked
        else []
    case (isEndGame board) of
        Just White -> putStrLn "\nGAME OVER\nWhite won"
        Just Black -> putStrLn "\nGAME OVER\nBlack won"
        Nothing ->
            let newSide = if newEnemies == [] then oppositeSide prevSide else prevSide 
                giveTurnAI = turnAI PvE newSide (currentPos,newEnemies) maxDepth board 
                giveTurnPlayer = turnPlayer PvE newSide (currentPos,newEnemies) maxDepth board
            in if newEnemies == [] then
                    if fromPlayer then giveTurnAI
                    else giveTurnPlayer
                else if fromPlayer then giveTurnPlayer
                    else giveTurnAI
                         
continueGameEvE :: Bool -> Pos -> Color ->Int -> Board -> IO ()
continueGameEvE attack currentPos prevSide maxDepth board = do
    let newEnemies = if attack then getEnemiesSeries currentPos board --get [Enemies] list of enemies at new position, that can be attacked
        else []
    case (isEndGame board) of
        Just White -> putStrLn "\nGAME OVER\nWhite won"
        Just Black -> putStrLn "\nGAME OVER\nBlack won"
        Nothing ->
            let newSide = if newEnemies == [] then oppositeSide prevSide else prevSide
            in turnAI EvE newSide (currentPos,newEnemies) maxDepth board

continueGamePvP :: Bool -> Pos -> Color -> Board -> IO () --Bool = True if previous move was attack, Pos where piece moved,
continueGamePvP attack currentPos prevSide board = do
    let newEnemies = if attack then getEnemiesSeries currentPos board  --get (newPos,[Enemies]) enemies at new position, that can be attacked
        else []
    case (isEndGame board) of
        Just White -> putStrLn "\nGAME OVER\nWhite won"
        Just Black -> putStrLn "\nGAME OVER\nBlack won"
        Nothing ->
            let newSide = if newEnemies == [] then oppositeSide prevSide else prevSide 
            in turnPlayer PvP newSide (currentPos,newEnemies) 0 board
test :: IO() --Testing "Series" attack
test = do
    let board = foldl (\board1 (from, to) -> moveTileGodMode from to board1) initialBoard
                [((1, 2), (4, 3)), ((6, 1), (5, 2)), ((6,5), (5,4)),((3,2),(8,2)),
                 ((2,5) ,(8,2)),((2,1) ,(8,2)), ((8,1), (3,4)), ((8,5) ,(3,2)), ((7,6) ,(3,6)),((6,7),(5,6)),((8,3),(8,8))]
    printBoard board
    turnPlayer PvP White ((-1,-1),[]) 0 board

testBoard :: Board
testBoard =foldl (\board1 (from, to) -> moveTileGodMode from to board1) initialBoard
                [((1, 2), (4, 3)), ((6, 1), (5, 2)), ((6,5), (5,4)),((3,2),(8,2)),
                 ((2,5) ,(8,2)),((2,1) ,(8,2)), ((8,1), (3,4)), ((8,5) ,(3,2)), ((7,6) ,(3,6)),((6,7),(5,6)),((8,3),(8,8))]
    

turnPlayer :: Mode -> Color -> (Pos,[Pos]) -> Int -> Board -> IO() --Turn of the player of given color. True if PvP.
turnPlayer mode side (_,[]) minimaxDepth board = do
    putStrLn $ show side ++ "'s move."
    putStrLn "List of tiles you can move (Row,Column): "
    let list = allMovesSide side board --Get List of all tiles you can move playing on chosen side
        --and their destinations
    showTilesToMove list
    putStr "\nWhat tile do you want to move? (Int): "
    posFrom <- getLine --Read index from user
    let index = read posFrom --Get Integer from posFrom
    let from = fst $ list !! (index-1)  --get first elem-get first element of (Pos,[Pos]) pair
    putStrLn "\nList of possible destinations (Row,Column): "
    showPossibleMoves from list
    putStr "\nWhere do you want to move? (Int): "
    posTo <-getLine --Read index from user
    let index = read posTo --Get integer from posTo
        to = (fromJust $ lookup from list) !! (index-1)  --get second element of (Pos,[Pos]) pair at index-1
        tileTo = getTile to board
        newPos = if isNothing tileTo then to --Just move piece
            else afterAttackPos from to --where piece will appear after attack
        newBoard = moveTile from to board
        attack = getColor to board == Just (oppositeSide side)
    printBoard newBoard
    case mode of
        PvP -> continueGamePvP attack newPos side newBoard
        PvE -> continueGamePvE attack True newPos side minimaxDepth newBoard
        _ -> error "wrong mode used"

turnPlayer mode side (currentPos,enemies) minimaxDepth board  = do
    putStrLn $ "Tile at " ++ show currentPos ++ " has to attack again. Attack one of these tiles: "
    showPossibleMoves currentPos [(currentPos,enemies)]
    putStr $ "Which tile to attack? (Int) "
    posTo <- getLine
    let index = read posTo
    let to = (fromJust $ lookup currentPos [(currentPos,enemies)] ) !! (index-1)
    putStrLn ""
    let newPos = afterAttackPos currentPos to  --where piece will appear after attack
    let newBoard = attackTile currentPos to board
    printBoard newBoard
    case mode of
        PvP -> continueGamePvP True newPos side newBoard
        PvE -> continueGamePvE True True newPos side minimaxDepth newBoard
        _ -> error "wrong mode used"

turnAI :: Mode -> Color -> (Pos,[Pos]) -> Int -> Board -> IO ()
turnAI mode side (currentPos,enemies) minimaxDepth board = do
    if enemies == [] then putStrLn $ show side ++ "'s move."
        else putStrLn $ show side ++ "'s SERIES ATTACK"
    let (((x1,y1),(x2,y2)),_) = minimax side ((-1,-1),(-1,-1)) (currentPos,enemies) 1 minimaxDepth board
        attack = getColor (x2,y2) board == Just (oppositeSide side)
        newPos = if attack then afterAttackPos (x1,y1) (x2,y2) --where piece will appear after attack
            else  (x2,y2) --Just move piece
        newBoard = moveTile (x1,y1) (x2,y2) board
    if (x2,y2)/=newPos
        then putStrLn $ "(" ++ (show x1) ++ "," ++ (show y1) ++ ") Attacks " 
        ++ "("++show x2 ++"," ++(show y2) ++")"
        else putStrLn $ "(" ++ (show x1) ++ "," ++ (show y1) ++ ") Goes to " 
        ++ "("++show x2 ++"," ++(show y2) ++")"
    printBoard newBoard
    case mode of
        PvE -> continueGamePvE attack False newPos side minimaxDepth newBoard
        EvE -> continueGameEvE attack newPos side minimaxDepth newBoard
        _ -> error "wrong mode used"

minimax ::Color -> (Pos,Pos)-> (Pos,[Pos]) -> Int -> Int -> Board -> ((Pos,Pos),Int)
minimax side (prevFrom,prevTo) (currentPos, enemies) depth maxDepth board | depth < maxDepth =
    case (isEndGame board) of
        Just White -> ((prevFrom,prevTo),1000) --White won
        Just Black -> ((prevFrom,prevTo),-1000) --Black won
        Nothing -> ((finalFrom,finalTo),val) where
            allPossibleMoves = if enemies ==[] then allMovesSide side board --get All Possible Moves for given Side
                else [(currentPos,enemies)] --It is Series attack. Enemies around
            moveVal pos posTo = number where--Get minimax value for given move
                    attack = getColor posTo board == Just (oppositeSide side)
                    newBoard = (moveTile pos posTo board)
                    newPos = if attack then afterAttackPos pos posTo else posTo
                    newEnemies = if attack then getEnemiesSeries newPos newBoard else []
                    newSide = if newEnemies==[] then (oppositeSide side) else side
                    newDepth = if newEnemies==[] then depth+1 else depth
                    ((_,_),number) = minimax newSide (pos,posTo) (newPos,newEnemies) newDepth maxDepth newBoard
            allResults = [ ( (pos, posTo), (moveVal pos posTo)) | 
                (pos, moves) <- allPossibleMoves, posTo <- moves
                ]
            ((finalFrom, finalTo), val) = if side==White 
                then maximumBy (comparing snd) allResults
                else minimumBy (comparing snd) allResults

minimax _ (prevFrom,prevTo) _ depth maxDepth board | depth >= maxDepth = ((prevFrom,prevTo),val) where
    val = case (isEndGame board) of
        Just White -> 1000 --White won
        Just Black -> -1000 --Black won
        Nothing -> evaluateBoard board

minimax _ _ _ _ _ _ = error "Error"

evaluateBoard :: Board -> Int
evaluateBoard board =
    let count color = sum [ if piece == Pawn then 1 else 2 | 
            Just (col, piece) <- concat board, col == color ]
    in count White - count Black 

isEndGame :: Board -> Maybe Color --Check if someone won
isEndGame board = 
    if onlyOneSide White board || allMovesSide Black board == [] then Just White
    else if onlyOneSide Black board || allMovesSide White board == [] then Just Black
    else Nothing 

emptyBoard :: Int -> Int -> Board --Create board axb size full of Empty Tiles
emptyBoard 0 0 = []
emptyBoard a b = replicate a (replicate b Nothing)

printBoard :: Board -> IO() 
printBoard board = do
    putStrLn "=================================="
    putStrLn ""
    putStr $ boardToString board
    putStrLn ""
    putStrLn "=================================="

initialBoard :: Board --Create initial board
initialBoard = [[initialTile r c | c <- [1 .. 8]] | r <- [1 .. 8]]

initialTile :: Int -> Int -> Tile --get Tile that has to be at position (r,c)
initialTile r c | r < 4 = if odd (r + c) then Just (White,Pawn) else Nothing
initialTile r c | r > 5 = if odd (r + c) then Just (Black,Pawn) else Nothing 
initialTile _ _ = Nothing

oppositeSide :: Color -> Color --Get the opposite player's color
oppositeSide White = Black
oppositeSide Black = White

setTile :: TilePos -> Board -> Board --set (tile,pos) at given board
setTile (tile,(x1, y1)) board =
    [[if x == x1 && y==y1 then tile else tileT | (y, tileT) <- [1..] `zip` row]
       | (x, row) <- [1..] `zip` board]

getColor :: Pos -> Board -> Maybe Color --Get color of tile at given position and board
getColor (r, c) board =  fmap fst tile
    where tile = board !! (r - 1) !! (c - 1)

getTile :: Pos -> Board -> Tile --Get color of tile at given position and board
getTile (r, c) board = board !! (r - 1) !! (c - 1)

moveTileGodMode :: Pos -> Pos -> Board -> Board --like MoveTile, but without checking the rules
moveTileGodMode from to board = setTile ((getTile from board), to) (setTile (Nothing,from) board)

moveTile :: Pos -> Pos -> Board -> Board --Move tile from one position to another
moveTile from to board =
    if getTile to board /= Nothing --enemy is at "to" position
        then attackTile from to board
        else if isNewKing to 
            then setNewKing to colorFrom (setTile (Nothing, from) board) -- move tile to empty position and make it a king
            else setTile (tileFrom,to) (setTile (Nothing,from) board) --Just move tile to empty position
    where tileFrom = getTile from board
          colorFrom = fromJust $ getColor from board

attackTile :: Pos -> Pos -> Board -> Board
attackTile from to board = newBoard where
     newPos = afterAttackPos from to --Where piece will appear after attack
     colorFrom =  fromJust $ getColor from board
     --set "from" to empty, set enemy to Empty, set tile at "from" to new position
     preBoard = setTile (Nothing,to) (setTile (Nothing, from) board)
     newTile = if isNewKing newPos then Just (colorFrom,King) else (getTile from board)
     newBoard = setTile (newTile,newPos) preBoard --createNewBoard

enemiesAround :: Pos -> Board -> [Pos] --Enemy Positions around given Tile for initial attack 
enemiesAround (r, c) board = [
    pos | pos <- [(r1,c1) | c1<-[c-1,c+1], r1<-map (\a -> r+a) dir],
    isInsideBoard pos, getColor pos board == Just (oppositeSide color)
    ]
    where player = getTile (r, c) board
          dir = playerDirs player 
          color = fromJust $ getColor (r,c) board  --We are sure that we call enemiesAround not on empty Tile

enemiesAroundSeries :: Pos -> Board -> [Pos] --Enemy Positions around given tile in series of attacks
enemiesAroundSeries (r,c) board = [
        (x1,y1)| y1<-[c-1,c+1], x1<-[r-1,r+1], 
        isInsideBoard (x1,y1), getColor (x1, y1) board == Just (oppositeSide color)
        ] 
    where color = fromJust $ getColor (r,c) board

afterAttackPos :: Pos -> Pos -> Pos -- return where attacking piece has to appear after attack
afterAttackPos (r,c) (rE,cE) = (rt,ct)
     --Attack has to continue in the same vector 
    where
        rt = if(rE<r) then rE-1  else rE+1
        ct = if(cE<c) then cE-1 else cE+1

allMovesFrom :: Pos -> Board -> [Pos] --List of all possible moves for the tile at given Pos
allMovesFrom (r, c) board = [ (r1,y1)| 
    y1<-[c-1,c+1], r1 <- map (\a ->r+a) dir,  --y1 is left and right, r1 is up or down
    isInsideBoard (r1,y1), getTile (r1, y1) board == Nothing
    ]
    where dir = playerDirs player
          player = getTile (r,c) board

allMovesSide ::  Color -> Board -> [(Pos,[Pos])] --List of all tiles possible to move at given side
allMovesSide side board = 
    if (allEnemies /= []) 
        then allEnemies --return list of all tiles that must attack and their enemies
        else [((r,c),list)|
            r<-[1..8],c<-[1..8], getColor (r,c) board == Just side, 
            let list = allMovesFrom (r,c) board, list /=[]
            ]
    where allEnemies = getEnemiesOfSide side board

getEnemiesSeries :: Pos -> Board -> [Pos] --Pos is Attacking tile, [Pos] is enemy positions available of series attack
getEnemiesSeries (r,c) board = [enemy | 
    let enemies = (enemiesAroundSeries (r,c) board), enemies/=[],
    let list = [(rt,ct) | 
            (rt,ct)<-enemies, let afterPos = afterAttackPos (r,c) (rt,ct), 
            isInsideBoard afterPos, getTile afterPos board == Nothing
            ],
    list/=[],
    enemy<-list
    ]

getEnemiesOfSide :: Color ->  Board -> [(Pos,[Pos])] --get list of all enemies of given side. Pos is attacking tile, [Pos] -enemies
getEnemiesOfSide side board = [((r,c),list)| 
    r<-[1..8],c<-[1..8], getColor (r,c) board == Just side, 
    let enemies = (enemiesAround (r,c) board), enemies/=[],
    let list = [(rt,ct) | 
            (rt,ct)<-enemies, let afterPos = afterAttackPos (r,c) (rt,ct), 
            isInsideBoard afterPos, getTile afterPos board == Nothing], list /=[]
    ]

isNewKing :: Pos -> Bool --True if tile at given Pos should be changed to King
isNewKing (8,_) = True
isNewKing (1,_) = True
isNewKing _ = False

setNewKing :: Pos -> Color -> Board -> Board --Create A King of given side at Pos
setNewKing (x,y) White board = setTile (Just (White,King),(x,y)) board
setNewKing (x,y) Black board = setTile (Just (Black, King),(x,y)) board

playerDirs :: Tile -> [Int]
playerDirs (Just (White,Pawn)) = [1]
playerDirs (Just (Black,Pawn)) = [-1]
playerDirs (Just (_,King)) = [1,-1]
playerDirs _ = error "bad argument"

showTilesToMove :: [(Pos,[Pos])] -> IO()
showTilesToMove [] = putStrLn ""
showTilesToMove list = mapM_ (\(((r,c),_),n)-> putStrLn 
    ("("++(show r)++ ","++(show c) ++ ") is " ++ (show n))) (zip list ([1..]::[Int]))

showPossibleMoves :: Pos -> [(Pos,[Pos])] -> IO()
showPossibleMoves _ [] = putStrLn ""
showPossibleMoves pos list =  mapM_ (\((r,c),n) -> putStrLn 
    ("("++(show r)++ ","++(show c) ++ ") is " ++ (show n))) 
    $(fromJust $ lookup pos list) `zip` [1..]

boardToString :: Board -> String
boardToString board = unlines("   1   2   3   4   5   6   7   8" : 
    [(show n) ++ "| " ++ intercalate " | " (List.map showTile row) ++ " |" | (row, n) <- board `zip` [1..]])

onlyOneSide :: Color -> Board -> Bool --Check if only given side is left on the board
onlyOneSide side board  = and [bool | r<-[1..8], c<-[1..8],
    let tile = getColor (r,c) board,
    let bool = (tile == Just side || tile==Nothing)]

isInsideBoard :: Pos -> Bool
isInsideBoard (r,c) = r>0 && r<9 && c>0 && c<9