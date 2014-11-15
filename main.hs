import System.IO ( BufferMode( NoBuffering )
                 , hSetEcho
                 , hSetBuffering
                 , stdout
                 , stdin)

import System.Process (system)

import Data.List ( delete
                 , sort)

-- |A Tuple containing two integers for x and y
type Coord = (Int,Int)

-- |A direction to move
data Dir = -- |Up
           North
           -- |Down
         | South
           -- |Right
         | East
           -- |Left
         | West

-- |An action command
data Command = -- |Move the worker in the given direction
               Move Dir
               -- |Quit the game
             | Quit

-- |A datatype for a sokoban level
data Warehouse =
  Warehouse { -- |Get the size of the warehouse (width, height)
              getSize    :: (Int,Int)
              -- |Get a list of all the places where a wall is
            , getWalls   :: [Coord]
              -- |Get a list of all the places where a crate is
            , getCrates  :: [Coord]
              -- |Get a list of all the places where a storage spot is
            , getStorage :: [Coord]
              -- |Get the current position of the worker
            , getWorker  :: Coord
              -- |Get the current amount of moves taken
            , getSteps   :: Int
            }

-- |Print a warehouse to the screen
instance Show Warehouse where
  show wh = unlines chars
    where
      -- |The size of the warehouse
      (maxX,maxY) = getSize wh
      -- |A two dimensional array of characters
      chars       = [[ showChar (x,y) | x <- [0..maxX]]
                                      | y <- [0..maxY]]
      -- |Convert a Coord to a character
      showChar coord
        -- |If the current coord is a crate in a storage space return *
        | isCrate wh coord   && isStorage wh coord = '*'
        -- |If the current coord is a worker on top of a storage space return +
        | isWorker wh coord  && isStorage wh coord = '+'
        -- |If the current coord is a wall return #
        | isWall wh coord    = '#'
        -- |If the current coord is a worker return @
        | isWorker wh coord  = '@'
        -- |If the current coord is a crate return o
        | isCrate wh coord   = 'o'
        -- |If the current coord is a storage space return .
        | isStorage wh coord = '.'
        -- |If there is nothing in the coord return a space
        | otherwise          = ' '


-- |Adds a direction to a coord
(+>) :: Coord -> Dir -> Coord
(+>) (x,y) dir =
  case dir of
    North -> (x,y-1)
    South -> (x,y+1)
    East  -> (x+1,y) 
    West  -> (x-1,y)


-- |Update a warehouse by moving in a given direction
move :: Warehouse -> Dir -> Warehouse
move wh dir
  -- |Hit a wall, no change
  | isWall wh newPos  = wh
  -- |Hit a crate
  | isCrate wh newPos =
    -- |If there is an object behind the crate
    if isCrate wh farPos || isWall wh farPos
      -- |Leave the crate alone
      then wh
      -- |Push the crate
      else wh''
  -- |Hit nothing, move along
  | otherwise = wh'
  where
    -- |Move a crate from the old coord to the new coord
    moveCrate old new = wh'{getCrates = new:delete old(getCrates wh')}
    -- |The workers current position
    oldPos  = getWorker wh
    -- |The position the worker wants to move
    newPos  = oldPos +> dir
    -- |The position a crate would get pushed to
    farPos  = newPos +> dir
    -- |Warehouse with updated worker position
    wh'     = wh { getWorker = newPos
                 , getSteps = getSteps wh + 1
                 }
    -- |Warehouse with updated crate position
    wh''    = moveCrate newPos farPos


-- |If the Coord is a wall inside the warehouse
isWall :: Warehouse -> Coord -> Bool
isWall wh coord = coord `elem` getWalls wh

-- |If the Coord is a crate inside the warehouse
isCrate :: Warehouse -> Coord -> Bool
isCrate wh coord = coord `elem` getCrates wh

-- |If the Coord is a storage spot inside the warehouse
isStorage :: Warehouse -> Coord -> Bool
isStorage wh coord = coord `elem` getStorage wh

-- |If the Coord is a worker inside the warehouse
isWorker :: Warehouse -> Coord -> Bool
isWorker wh coord = getWorker wh == coord

-- |If the all the crates are in storage spots
isFinished :: Warehouse -> Bool
isFinished wh = sort (getCrates wh) == sort (getStorage wh)

-- |Load a level into memory
loadLevel :: String -> Warehouse
loadLevel str =
  -- |Start with an empty warehouse and fold over all the elements in the string
  foldl consume (emptyWarehouse{getSize=size}) elems
  where
    -- |An Infinite two dimensional array
    coords = [[(x,y) | x <- [0..]]
                     | y <- [0..]]
    -- |Assign a coord to each element in str
    elems  = concat $ zipWith zip coords $ lines str
    -- |The maximum x in the generated elements
    maxX   = maximum . map (fst . fst) $ elems
    -- |The maximum y in the generated elements
    maxY   = maximum . map (snd . fst) $ elems
    -- |The size of the warehouse
    size   = (maxX,maxY)
    -- |Takes an element and its position and updates the warehouse
    consume wh (coord, elem) =
      case elem of
        -- |Set the current worker position
        '@' -> wh{ getWorker  = coord}
        -- |Add a crate to the current position
        'o' -> wh{ getCrates  = coord:getCrates wh}
        -- |Add a wall to the current position
        '#' -> wh{ getWalls   = coord:getWalls wh}
        -- |Add a storage spot to the current position
        '.' -> wh{ getStorage = coord:getStorage wh}
        -- |Add a crate and a storage spot to the current position
        '*' -> wh{ getCrates  = coord:getCrates wh
                 , getStorage = coord:getStorage wh}
        -- |Set the worker and add a storage spot to the current position
        '+' -> wh{ getWorker  = coord
                 , getStorage = coord:getStorage wh}
        -- |Add nothing
        ' ' -> wh
        -- |What is this?
        otherwise -> error $ "invalid token "++ show elem

-- |An empty warehouse
emptyWarehouse = Warehouse { getWalls   = []
                           , getCrates  = []
                           , getStorage = []
                           , getSize    = (0,0)
                           , getWorker  = (0,0)
                           , getSteps   = 0
                           }

-- |Load a warehouse from a filepath
loadFromFile :: FilePath -> IO Warehouse
loadFromFile fp = do
  -- |Load the file
  stuff <- readFile fp
  -- |Load the warehouse and raise it into the IO monad
  return $ loadLevel stuff

-- |Get a command from the user
getInput :: IO Command
getInput = do
  char <- getChar
  case char of
    'w' -> return $ Move North
    's' -> return $ Move South
    'a' -> return $ Move West
    'd' -> return $ Move East
    'q' -> return Quit
    _   -> getInput

-- |Initialize the game and start the mainloop
main = do
  -- |Initialize the terminal
  -- |Remove the need for the user to hit enter
  hSetBuffering stdin NoBuffering
  -- |Make output instant
  hSetBuffering stdout NoBuffering
  -- |Don't echo the characters
  hSetEcho stdout False

  -- |Load the level
  level <- loadFromFile "level"
  -- |Start the alt screen
  system "tput smcup"
  -- |Start the mainloop
  mainloop level

-- |Takes a warehouse and does IO things
mainloop :: Warehouse -> IO ()
mainloop wh = do
  -- |Set the cursor to the top left
  system "tput cup 0 0"
  -- |Print out the current warehouse layout
  putStr $ show wh
  -- |Print out the instructions
  putStr $ "wasd to move, q to quit"

  -- |Get a command from the user
  cmd <- getInput
  case cmd of
    Quit -> do
      -- |End the alt screen
      system "tput rmcup"
      -- |Exit the mainloop
      return ()

    Move a -> do
      -- |Update the warehouse
      let wh' = move wh a
      if isFinished wh'
        then do
          -- |End the alt screen
          system "tput rmcup"
          -- |Print your score
          putStrLn $ "You finished in "++show (getSteps wh')++" steps!"

        -- |Keep looping
        else mainloop wh'
