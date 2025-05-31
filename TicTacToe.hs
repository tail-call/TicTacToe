{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}       -- For GHC.Generics to derive Binary instances
{-# LANGUAGE StandaloneDeriving #-}  -- For deriving instances for GameStatus
{-# LANGUAGE FlexibleInstances #-}   -- Might be needed for Binary on GameStatus if Generic doesn't suffice

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (transpose, reverse) -- Explicitly import reverse for clarity with board transformations
import Data.Maybe (isJust, isNothing)
import Data.Map (Map)
import qualified Data.Map as M
import System.IO (hPutStrLn, stderr)
import Data.Binary (Binary, encode, decode)
import GHC.Generics (Generic)        -- For deriving Generic for Binary
import qualified Data.ByteString.Lazy as BL (writeFile, readFile, null)
import System.Random (StdGen, newStdGen, randomR, mkStdGen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (when)          -- For debugging or conditional actions
import System.IO.Error (catchIOError) -- For catching file errors

-- # Data Types

-- |Shape kind (X or O)
data Shape = X | O deriving (Eq, Show, Ord, Generic) -- Added Ord for Map key, Generic for Binary

-- |State of a cell on the board
data Cell = Empty | Taken Shape deriving (Eq, Show, Ord, Generic) -- Added Ord for Map key, Generic for Binary

-- |Board where shapes are placed in cells.
type Board = [[Cell]]

-- |The game board is a 3x3 grid of cells
initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Empty)

-- |Status of the game: playing, win or draw
data GameStatus = Playing | XWins | OWins | Draw deriving (Eq, Show, Ord, Generic) -- Added Ord for Map key, Generic for Binary

-- Data for AI learning: maps a canonical board state to its estimated value
type ValueMap = Map Board Double

-- |TicTacToes's game state
data GameState = GameState
    { board             :: Board
    , currentPlayer     :: Shape
    , gameStatus        :: GameStatus
    , aiExplorationRate :: Double   -- Epsilon for epsilon-greedy (e.g., 0.1 for 10% random moves)
    , aiLearningRate    :: Double   -- Alpha for Q-learning update (e.g., 0.7)
    , aiPlayer          :: Shape      -- Which player is the AI (X or O)
    , randomGen         :: StdGen     -- Random number generator for AI's exploration
    , gameHistory       :: [(Board, Shape)] -- Stores (board_state_before_move, player_who_moved_into_that_state)
    } deriving (Eq, Show, Generic) -- Generic for easy deriving of instances if needed, but StdGen prevents full Binary.

-- Derive Binary instances for data types used in ValueMap
instance Binary Shape
instance Binary Cell
instance Binary GameStatus

-- AI value file path
aiValueFile :: String
aiValueFile = "ai_values.dat"

-- | Loads the AI's learned values from a file. Returns an empty map if file not found or corrupted.
loadAIValues :: IO ValueMap
loadAIValues = do
    content <- BL.readFile aiValueFile `catchIOError` \e -> do
        hPutStrLn stderr $ "Warning: Could not load " ++ aiValueFile ++ ": " ++ show e ++ ". Starting with empty AI knowledge."
        return BL.empty
    if BL.null content
        then return M.empty
        else return $ decode content

-- | Saves the AI's learned values to a file.
saveAIValues :: ValueMap -> IO ()
saveAIValues values = BL.writeFile aiValueFile (encode values)

-- |The starting state of the game (will be further initialized in `main`)
initialState :: GameState
initialState = GameState
    { board             = initialBoard
    , currentPlayer     = X
    , gameStatus        = Playing
    , aiExplorationRate = 0.1   -- 10% chance for AI to make a random move
    , aiLearningRate    = 0.7   -- High learning rate for quick adaptation
    , aiPlayer          = O     -- AI plays as O
    , randomGen         = mkStdGen 42 -- Dummy initial value, replaced by newStdGen in main
    , gameHistory       = []    -- History is empty at the start of a new game
    }

-- # Game Logic

-- |Function to place a mark on the board.
-- Returns `Just newBoard` if successful, `Nothing` if the cell is already taken or out of bounds
placeMark :: Board -> Shape -> Int -> Int -> Maybe Board
placeMark currentBoard player row col
    | row < 0 || row >= 3 || col < 0 || col >= 3 = Nothing -- Out of bounds
    | (currentBoard !! row) !! col == Empty      = Just $
        take row currentBoard ++
        [ take col (currentBoard !! row) ++ [Taken player] ++ drop (col + 1) (currentBoard !! row) ] ++
        drop (row + 1) currentBoard
    | otherwise                                  = Nothing -- Cell already taken

-- |Check for a win condition for a given player
checkWin :: Board -> Shape -> Bool
checkWin b p = any (all (== Taken p)) (rows ++ cols ++ diags)
  where
    rows  = b
    cols  = transpose b
    diags = [[b !! 0 !! 0, b !! 1 !! 1, b !! 2 !! 2], -- Main diagonal
             [b !! 0 !! 2, b !! 1 !! 1, b !! 2 !! 0]] -- Anti-diagonal

-- |Check for a draw condition
checkDraw :: Board -> Bool
checkDraw b = all (notElem Empty) b -- All cells are taken
              && not (checkWin b X || checkWin b O) -- And no one has won

-- |Update game status based on the board
updateGameStatus :: GameState -> GameState
updateGameStatus gs =
    let currentB = board gs
    in case gameStatus gs of
        Playing ->
            if checkWin currentB X
                then gs { gameStatus = XWins }
                else if checkWin currentB O
                    then gs { gameStatus = OWins }
                    else if checkDraw currentB
                        then gs { gameStatus = Draw }
                        else gs -- Still playing
        _ -> gs -- Game already ended, no status change

-- | Apply a game move (either human or AI) and update game state and history.
-- This function processes the actual board change, updates whose turn it is,
-- records the state in `gameHistory`, and updates the game status.
applyMove :: GameState -> Int -> Int -> GameState
applyMove gs row col =
    let currentB = board gs
        player = currentPlayer gs
        history = gameHistory gs
    in case placeMark currentB player row col of
        Just newBoard ->
            let nextPlayer = if player == X then O else X
                -- Record the board state *before* the move, and the player who just moved
                updatedGs = gs { board = newBoard, currentPlayer = nextPlayer, gameHistory = history ++ [(currentB, player)] }
            in updateGameStatus updatedGs
        Nothing       -> gs -- Invalid move (cell taken or out of bounds), return original state

-- # AI Learning and Decision Logic

-- | Generate all symmetric variants (rotations and flips) of a board.
boardSymmetries :: Board -> [Board]
boardSymmetries b =
    [ b
    , rotateBoard b
    , rotateBoard (rotateBoard b)
    , rotateBoard (rotateBoard (rotateBoard b))
    , flipBoardHorizontal b
    , rotateBoard (flipBoardHorizontal b)
    , rotateBoard (rotateBoard (flipBoardHorizontal b))
    , rotateBoard (rotateBoard (rotateBoard (flipBoardHorizontal b)))
    ]

-- | Rotate a board 90 degrees clockwise.
rotateBoard :: Board -> Board
rotateBoard b = transpose (map reverse b)

-- | Flip a board horizontally.
flipBoardHorizontal :: Board -> Board
flipBoardHorizontal b = map reverse b

-- | Get the canonical representation of a board (lexicographically smallest among all symmetries).
-- This is crucial for efficient learning as it reduces the number of unique states the AI needs to learn.
canonicalBoard :: Board -> Board
canonicalBoard b = minimum (boardSymmetries b)

-- | Get all empty cells on the board as (row, col) coordinates.
getEmptyCells :: Board -> [(Int, Int)]
getEmptyCells b = [(r, c) | r <- [0..2], c <- [0..2], (b !! r !! c) == Empty]

-- | Simulate placing a mark on the board without modifying the original (for AI lookahead).
simulatePlaceMark :: Board -> Shape -> Int -> Int -> Board
simulatePlaceMark currentBoard player row col =
    -- This uses the same board manipulation logic as `placeMark` but assumes the move is valid
    take row currentBoard ++
    [ take col (currentBoard !! row) ++ [Taken player] ++ drop (col + 1) (currentBoard !! row) ] ++
    drop (row + 1) currentBoard

-- | Get the estimated value of a board state from the AI's perspective (0.0 to 1.0).
-- Value 1.0 means AI wins, 0.0 means AI loses, 0.5 means draw/unknown.
getBoardValue :: ValueMap -> Shape -> Board -> Double
getBoardValue valMap aiPlayerShape board =
    let cb = canonicalBoard board
        -- Default to 0.5 for unknown states (neutral/draw probability)
        val = M.findWithDefault 0.5 cb valMap
    in if aiPlayerShape == X then val else 1.0 - val -- If AI is O, X's win is O's loss, so flip the value

-- | Make the AI's move based on learned values and exploration strategy.
makeAIMove :: ValueMap -> GameState -> GameState
makeAIMove valMap gs =
    let currentB = board gs
        aiP = aiPlayer gs
        (explorationRand, g') = randomR (0.0, 1.0) (randomGen gs) -- Generate random value for exploration
        possibleMoves = getEmptyCells currentB

    in if null possibleMoves
        then gs { randomGen = g' } -- No moves possible (game likely over or draw), update random generator and return state
        else
            let -- Calculate values for all possible next states
                -- For each potential move (r, c), simulate the board state *after* AI makes the move.
                -- Then evaluate that resulting board state from AI's perspective using `getBoardValue`.
                evaluatedMoves =
                    [ (getBoardValue valMap aiP nextB, r, c)
                    | (r, c) <- possibleMoves
                    , let nextB = simulatePlaceMark currentB aiP r c
                    ]

                -- Find the maximum value among all evaluated moves
                bestMoveValue = maximum (map (\(val,_,_) -> val) evaluatedMoves)
                -- Filter to get all moves that yield this best value
                bestMoves = filter (\(val,_,_) -> val == bestMoveValue) evaluatedMoves

                -- Epsilon-greedy strategy:
                -- With probability `aiExplorationRate`, pick a random move.
                -- Otherwise (exploitation), pick one of the best moves.
                chosenCell :: (Int, Int)
                chosenCell = if explorationRand < aiExplorationRate gs
                                then possibleMoves !! (fst (randomR (0, length possibleMoves - 1) g')) -- Pick a random available cell
                                else let (_, r_best, c_best) = head bestMoves -- Pick the first among equally best moves
                                     in (r_best, c_best)

                -- Apply the chosen AI move to the game state
                updatedGs = applyMove gs (fst chosenCell) (snd chosenCell)
            in updatedGs { randomGen = g' } -- Update random generator in the state

-- | Learn from a completed game by updating the `ValueMap` based on the final outcome.
-- This function back-propagates the reward through the game history.
learnFromGame :: GameStatus -> [(Board, Shape)] -> Shape -> ValueMap -> Double -> ValueMap
learnFromGame finalStatus history aiP valMap learningRate =
    let -- Determine the numerical reward for the AI based on the game's final status
        reward = case finalStatus of
            XWins -> if aiP == X then 1.0 else 0.0 -- AI X wins -> 1.0, AI X loses -> 0.0
            OWins -> if aiP == O then 1.0 else 0.0 -- AI O wins -> 1.0, AI O loses -> 0.0
            Draw  -> 0.5                          -- Draw is a neutral outcome
            _     -> 0.5                          -- Should not happen for a finished game (default to draw value)

        -- Helper function to update a single state's value
        updateSingleValue :: ValueMap -> (Board, Shape) -> ValueMap
        updateSingleValue currentMap (b, playerWhoMoved) =
            let cb = canonicalBoard b
                currentVal = M.findWithDefault 0.5 cb currentMap -- Get current learned value for this state
                -- Determine the target value for this state:
                -- If the AI made the move that led to `b`, the target is the final `reward`.
                -- If the opponent made the move that led to `b`, the target is the inverse of the `reward`
                -- (because opponent's win is AI's loss, and vice-versa).
                targetVal = if aiP == playerWhoMoved then reward else 1.0 - reward
                -- Update value using Q-learning formula: V(s) = V(s) + alpha * (target_value - V(s))
                newVal = currentVal + learningRate * (targetVal - currentVal)
            in M.insert cb newVal currentMap

    -- Apply updates to all states in the game history, processing from end to beginning.
    -- Reversing the history ensures that states closer to the end of the game are updated first,
    -- allowing their new values to influence updates for earlier states in the next iteration (if applicable).
    in foldl updateSingleValue valMap (reverse history)

-- # Graphics Constants (unchanged)

width, height :: Int
width = 600
height = 600

window :: Display
window = InWindow "Tic-Tac-Toe" (width, height) (100, 100)

backgroundColor :: Color
backgroundColor = greyN 0.1

lineColor :: Color
lineColor = white

xColor :: Color
xColor = blue

oColor :: Color
oColor = red

statusColor :: Color
statusColor = yellow

boardSize :: Float
boardSize = fromIntegral (min width height) * 0.7

cellSize :: Float
cellSize = boardSize / 3.0

halfBoard :: Float
halfBoard = boardSize / 2.0

-- # Drawing Functions (unchanged)

drawGrid :: Picture
drawGrid = Color lineColor . Pictures $
    [ Line [(-halfBoard, cellSize / 2), (halfBoard, cellSize / 2)]
    , Line [(-halfBoard,- (cellSize / 2)), (halfBoard,- (cellSize / 2))]
    , Line [(cellSize / 2, -halfBoard), (cellSize / 2, halfBoard)]
    , Line [(- (cellSize / 2), -halfBoard), (- (cellSize / 2), halfBoard)]
    ]

drawX :: Point -> Picture
drawX (cx, cy) = Color xColor . Pictures $
    [ Line [(cx - markSize, cy - markSize), (cx + markSize, cy + markSize)]
    , Line [(cx - markSize, cy + markSize), (cx + markSize, cy - markSize)]
    ]
  where
    markSize = cellSize / 3.0

drawO :: Point -> Picture
drawO (cx, cy) = Color oColor $
    Translate cx cy (ThickCircle (cellSize / 3.0) (cellSize / 8.0))

drawCell :: Cell -> Point -> Picture
drawCell Empty      _ = Blank
drawCell (Taken X)  p = drawX p
drawCell (Taken O)  p = drawO p

cellToCoords :: Int -> Int -> Point
cellToCoords row col = (x, y)
  where
    x = fromIntegral col * cellSize - halfBoard + cellSize / 2.0
    y = fromIntegral row * (-cellSize) + halfBoard - cellSize / 2.0

drawBoard :: Board -> Picture
drawBoard b = Pictures $
    [ drawCell (b !! r !! c) (cellToCoords r c)
    | r <- [0..2]
    , c <- [0..2]
    ]

drawStatus :: GameState -> Picture
drawStatus gs = Translate (-200) (halfBoard + 50) $ Scale 0.2 0.2 $ Color statusColor $
    case gameStatus gs of
        Playing -> Text $ "Current turn: " <> (if currentPlayer gs == X then "X" else "O")
        XWins   -> Text "X WINS!"
        OWins   -> Text "O WINS!"
        Draw    -> Text "IT'S A DRAW!"

displayPicture :: GameState -> Picture
displayPicture gs = Pictures
    [ drawGrid
    , drawBoard (board gs)
    , drawStatus gs
    ]

-- # Event Handling

-- | Convert screen coordinates to board indices (row, col)
-- Returns Nothing if outside the board area
coordsToCell :: Point -> Maybe (Int, Int)
coordsToCell (mx, my)
    | abs mx > halfBoard || abs my > halfBoard = Nothing -- Click outside board
    | otherwise = Just (row, col)
  where
    -- Adjust coords so (0,0) is top-left of board section
    adjustedX = mx + halfBoard
    adjustedY = halfBoard - my -- Y is inverted for board rows
    row = floor (adjustedY / cellSize)
    col = floor (adjustedX / cellSize)

-- |Handle incoming events (mouse clicks, key presses) for the `playIO` function.
-- This function now takes an `IORef ValueMap` to allow mutable access to the AI's knowledge.
handleEvent :: IORef ValueMap -> Event -> GameState -> IO GameState
handleEvent valRef (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs = do
    currentValMap <- readIORef valRef -- Read the current AI knowledge

    case gameStatus gs of
        Playing ->
            -- Try to process a human click
            case coordsToCell (mx, my) of
                Just (row, col) -> do
                    let gsAfterHumanMove = applyMove gs row col -- Apply human's move
                    case gameStatus gsAfterHumanMove of
                        Playing -> -- Game continues after human move
                            if currentPlayer gsAfterHumanMove == aiPlayer gsAfterHumanMove
                                then do
                                    -- It's AI's turn, make AI move immediately
                                    let gsAfterAIMove = makeAIMove currentValMap gsAfterHumanMove
                                    case gameStatus gsAfterAIMove of
                                        Playing -> return gsAfterAIMove -- AI moved, game continues
                                        _       -> do -- AI moved, and the game ended (AI won or drew)
                                            -- Learn from the completed game
                                            let newValMap = learnFromGame (gameStatus gsAfterAIMove) (gameHistory gsAfterAIMove) (aiPlayer gsAfterAIMove) currentValMap (aiLearningRate gsAfterAIMove)
                                            writeIORef valRef newValMap -- Update the IORef
                                            saveAIValues newValMap      -- Save to file
                                            -- Start a new game, keeping AI's random generator and player side
                                            return $ initialState { randomGen = randomGen gsAfterAIMove, aiPlayer = aiPlayer gsAfterAIMove }
                                else return gsAfterHumanMove -- Human moved, and it's still human's turn (e.g., if AI is X and human just moved as O)
                        _ -> do -- Human moved, and the game ended (human won or drew)
                            -- Learn from the completed game
                            let newValMap = learnFromGame (gameStatus gsAfterHumanMove) (gameHistory gsAfterHumanMove) (aiPlayer gsAfterHumanMove) currentValMap (aiLearningRate gsAfterHumanMove)
                            writeIORef valRef newValMap -- Update the IORef
                            saveAIValues newValMap      -- Save to file
                            -- Start a new game, keeping AI's random generator and player side
                            return $ initialState { randomGen = randomGen gsAfterHumanMove, aiPlayer = aiPlayer gsAfterHumanMove }
                Nothing -> return gs -- Click outside board, no change
        _ -> return gs -- Game already ended, no move allowed
handleEvent valRef (EventKey (Char 'r') Down _ _) gs = do
    -- Reset game with 'r' key. The AI's learned values are already in the IORef.
    -- A new game starts with an empty history.
    return $ initialState { randomGen = randomGen gs, aiPlayer = aiPlayer gs }
handleEvent _ _ gs = return gs -- Ignore other events

-- | The update function for Gloss.
-- In this turn-based game, AI moves happen immediately within `handleEvent`,
-- so this function just passes the state through.
updateGameTime :: Float -> IORef ValueMap -> GameState -> IO GameState
updateGameTime _ _ gs = return gs

-- # Main Function

main :: IO ()
main = do
    -- Load AI's learned values from the file on startup
    aiVals <- loadAIValues
    -- Create a new random generator for the current game session
    g      <- newStdGen
    -- Create an IORef to hold the mutable AI value map, initialized with loaded values
    valRef <- newIORef aiVals

    -- Initialize the game state, setting the random generator
    let initialPlayState = initialState { randomGen = g }

    -- Run the Gloss simulation using `playIO` for IO capabilities
    playIO
        window                  -- The display window
        backgroundColor         -- The background color
        60                      -- The number of simulation steps per second (frame rate)
        initialPlayState        -- The initial game state
        displayPicture          -- Function to convert game state to a picture
        (handleEvent valRef)    -- Function to handle input events, partially applied with the IORef
        (updateGameTime valRef) -- Function to update the game state over time, partially applied