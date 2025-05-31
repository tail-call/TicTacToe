-- TicTacToe.hs

{-# LANGUAGE OverloadedStrings #-} -- XXX ???

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (transpose)
import Data.Maybe (isJust, isNothing)

-- # Data Types

-- |Shape kind (X or O)
data Shape = X | O deriving (Eq, Show)


-- |State of a cell on the board
data Cell = Empty | Taken Shape deriving (Eq, Show)

-- |Board where shapes are placed in cells.
type Board = [[Cell]]

-- |The game board is a 3x3 grid of cells
initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Empty)

-- |Status of the game: playing, win or draw
data GameStatus = Playing | XWins | OWins | Draw deriving (Eq, Show)

-- |TicTacToes's game state
data GameState = GameState
    { -- |The current 3x3 board
      board       :: Board
      -- |Whose turn it is
    , currentPlayer :: Shape
      -- |Current status of the game
    , gameStatus  :: GameStatus
    } deriving (Eq, Show)

-- |The starting state of the game
initialState :: GameState
initialState = GameState
    { board       = initialBoard
    , currentPlayer = X
    , gameStatus  = Playing
    }

-- # Game Logic

-- |Function to place a mark on the board.
-- 
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
    diags = [[head (head b), b !! 1 !! 1, b !! 2 !! 2],
             [head b !! 2, b !! 1 !! 1, head (b !! 2)]]

-- |Check for a draw condition
checkDraw :: Board -> Bool
checkDraw b = all (notElem Empty) b -- All cells are taken
              && not (checkWin b X || checkWin b O) -- And no one has won

-- |Update game status based on the board
updateGameStatus :: GameState -> GameState
updateGameStatus gs =
    let currentB = board gs
        nextP = currentPlayer gs
        status = gameStatus gs
    in case status of
        Playing ->
            if checkWin currentB X
                then gs { gameStatus = XWins }
                else if checkWin currentB O
                    then gs { gameStatus = OWins }
                    else if checkDraw currentB
                        then gs { gameStatus = Draw }
                        else gs -- Still playing
        _ -> gs -- Game already ended, no status change

-- # Graphics Constants

-- |Window size
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

-- |Board dimensions
boardSize :: Float
boardSize = fromIntegral (min width height) * 0.7 -- Make board 70% of window size

cellSize :: Float
cellSize = boardSize / 3.0

-- |Half dimensions for positioning relative to center
halfBoard :: Float
halfBoard = boardSize / 2.0

-- # Drawing Functions

-- |Draws the Tic-Tac-Toe grid lines
drawGrid :: Picture
drawGrid = Color lineColor . Pictures $
    [ Line [(-halfBoard, cellSize / 2), (halfBoard, cellSize / 2)]    -- Top horizontal
    , Line [(-halfBoard,- (cellSize / 2)), (halfBoard,- (cellSize / 2))] -- Bottom horizontal
    , Line [(cellSize / 2, -halfBoard), (cellSize / 2, halfBoard)]   -- Right vertical
    , Line [(- (cellSize / 2), -halfBoard), (- (cellSize / 2), halfBoard)] -- Left vertical
    ]

-- |Draw an 'X' at a given center point (x, y)
drawX :: Point -> Picture
drawX (cx, cy) = Color xColor . Pictures $
    [ Line [(cx - markSize, cy - markSize), (cx + markSize, cy + markSize)]
    , Line [(cx - markSize, cy + markSize), (cx + markSize, cy - markSize)]
    ]
  where
    markSize = cellSize / 3.0 -- Size of the X relative to cell

-- |Draw an 'O' at a given center point (x, y)
drawO :: Point -> Picture
drawO (cx, cy) = Color oColor $
    Translate cx cy (ThickCircle (cellSize / 3.0) (cellSize / 8.0)) -- Radius, thickness

-- |Draw a single cell based on its content (Empty, Taken X, Taken O)
drawCell :: Cell -> Point -> Picture
drawCell Empty      _ = Blank
drawCell (Taken X)  p = drawX p
drawCell (Taken O)  p = drawO p

-- | Convert board (row, col) indices to screen coordinates:
--     (x, y) (0,0) is top-left;
--     (2,2) is bottom-right.
--
-- For board indices:
--   Screen (0,0) is center;
--   top-left is (-halfBoard, halfBoard).
cellToCoords :: Int -> Int -> Point
cellToCoords row col = (x, y)
  where
    x = fromIntegral col * cellSize - halfBoard + cellSize / 2.0
    y = fromIntegral row * (-cellSize) + halfBoard - cellSize / 2.0

-- |Draw the entire board based on the GameState
drawBoard :: Board -> Picture
drawBoard b = Pictures $
    [ drawCell (b !! r !! c) (cellToCoords r c)
    | r <- [0..2]
    , c <- [0..2]
    ]

-- |Draw the current game status message
drawStatus :: GameState -> Picture
drawStatus gs = Translate (-200) (halfBoard + 50) $ Scale 0.2 0.2 $ Color statusColor $
    case gameStatus gs of
        Playing -> Text $ "Current turn: " <> (if currentPlayer gs == X then "X" else "O")
        XWins   -> Text "X WINS!"
        OWins   -> Text "O WINS!"
        Draw    -> Text "IT'S A DRAW!"

-- |The main rendering function
displayPicture :: GameState -> Picture
displayPicture gs = Pictures
    [ drawGrid
    , drawBoard (board gs)
    , drawStatus gs
    ]

-- # Event Handling

-- | Convert screen coordinates to board indices (row, col)
--
--   Returns Nothing if outside the board area
coordsToCell :: Point -> Maybe (Int, Int)
coordsToCell (mx, my)
    | abs mx > halfBoard || abs my > halfBoard = Nothing -- Click outside board
    | otherwise = Just (row, col)
  where
    -- Adjust coords so (0,0) is top-left of board
    adjustedX = mx + halfBoard
    adjustedY = halfBoard - my -- Y is inverted for board rows
    row = floor (adjustedY / cellSize)
    col = floor (adjustedX / cellSize)

-- |Handle incoming events (mouse clicks, key presses)
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs =
    case gameStatus gs of
        Playing ->
            case coordsToCell (mx, my) of
                Just (row, col) ->
                    case placeMark (board gs) (currentPlayer gs) row col of
                        Just newBoard ->
                            let nextPlayer = if currentPlayer gs == X then O else X
                                updatedGs = gs { board = newBoard, currentPlayer = nextPlayer }
                            in updateGameStatus updatedGs
                        Nothing       -> gs -- Cell already taken, no change
                Nothing -> gs -- Click outside board, no change
        _ -> gs -- Game not playing, no move allowed
handleEvent (EventKey (Char 'r') Down _ _) _ = initialState -- Reset game with 'r' key
handleEvent _ gs = gs -- Ignore other events

-- 7. Main Function

main :: IO ()
main = play
    window                  -- The display window
    backgroundColor         -- The background color
    60                      -- The number of simulation steps per second (not used in this turn-based game, but required)
    initialState            -- The initial game state
    displayPicture          -- The function to convert game state to a picture
    handleEvent             -- The function to handle input events
    (\_ -> id)              -- The function to update the game state over time (not used here)
