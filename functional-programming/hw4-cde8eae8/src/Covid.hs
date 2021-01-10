module Covid 
  ( CovidGrid(..)
  , nextGrid
  , single
  , InfectionInfo(..)
  ) where

import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Comonad
import System.Random

data ListZipper a = LZ [a] a [a] deriving Show

safeListLeft, safeListRight :: ListZipper a -> Maybe (ListZipper a)
safeListLeft (LZ [] _ _) = Nothing
safeListLeft (LZ (a:as) x bs) = Just $ LZ as a (x:bs)

safeListRight (LZ _ _ []) = Nothing
safeListRight (LZ as x (b:bs)) = Just $ LZ (x:as) b bs

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> Maybe a) -> a -> [a] 
iterateTail f start = 
  catMaybes $ takeWhile isJust $ tail infinityList
    where 
      infinityList = iterate (f . fromJust) (Just start)

mkZipper :: (v -> Maybe v) -> (v -> Maybe v) -> v -> ListZipper v
mkZipper genLeft genRight e = 
  LZ (iterateTail genLeft e) e (iterateTail genRight e)

instance Comonad ListZipper where
  extract (LZ _ x _) = x

  duplicate = mkZipper safeListLeft safeListRight

data CellState 
  = Healthy 
  | Incubation
  | Sick
  | Immune

data Cell 
  = Cell 
  { nDays :: Int
  , cellState :: CellState
  , cellRandomGenerator :: StdGen
  } 

data InfectionInfo
  = InfectionInfo
  { infectionP :: Double
  , incubationPeriod :: Int
  , infectiousPeriod :: Int
  , immunityPeriod :: Int
  }

newtype Grid a = 
  Grid { unGrid :: ListZipper ( ListZipper a ) } deriving Show

instance Comonad Grid where
  extract = gridRead

  duplicate = Grid . fmap horizontal . vertical

up, down :: Grid a -> Maybe (Grid a)
up   (Grid g) = Grid <$> safeListLeft g
down (Grid g) = Grid <$> safeListRight g

left, right :: Grid a -> Maybe (Grid a)
left  (Grid g) = 
  case row of 
    Nothing -> Nothing
    Just _  -> Just $ Grid $ fmap fromJust rows
  where 
    rows = fmap safeListLeft g 
    row = extract rows
right (Grid g) = 
  case row of 
    Nothing -> Nothing
    Just _  -> Just $ Grid $ fmap fromJust rows
  where 
    rows = fmap safeListRight g 
    row = extract rows

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g
 
gridUpdate :: (a -> a) -> Grid a -> Grid a
gridUpdate f grid@(Grid g) = Grid $ listWrite newLine g
  where
    oldValue = extract grid
    oldLine = extract g
    newLine = listWrite (f oldValue) oldLine


horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper left right
vertical   = mkZipper up down

instance Functor Grid where
  fmap f (Grid lz) = Grid $ fmap (fmap f) lz

data CovidGrid = CovidGrid (Grid Cell) InfectionInfo

newState :: CellState -> Cell -> Cell
newState newState cell = 
  cell { nDays = 0, cellState = newState }

updateCell :: Cell -> Cell
updateCell cell = 
  cell { nDays = nDays cell + 1 }

isInfected :: Cell -> Bool
isInfected cell = 
  case cellState cell of
    Incubation -> True
    Sick       -> True
    _          -> False

randomProbability :: RandomGen g => g -> (Double, g)
randomProbability = random

cellProbability :: Cell -> (Double, Cell)
cellProbability cell = (p, cell { cellRandomGenerator = g })
  where (p, g) = randomProbability $ cellRandomGenerator cell

infectedNeighbours :: Grid Cell -> Int
infectedNeighbours grid = length $ filter isInfected neighbours
  where
    neighbours :: [Cell]
    neighbours = 
        map extract $ 
          mapMaybe (\f -> f grid) (horizontals ++ verticals ++ 
            liftM2 (\h v x -> v =<< h x) horizontals verticals)
      where horizontals = [Covid.left, Covid.right]
            verticals   = [up, down]


single :: InfectionInfo -> Int -> Int -> Int -> CovidGrid
single infection w h seed = CovidGrid grid infection
  where 
    rH = (h - 1) `div` 2
    rH' = rH + (1 - h `mod` 2)
    rW = (w - 1) `div` 2
    rW' = rW + (1 - w `mod` 2)

    rawCell :: Int -> Cell
    rawCell genSeed = 
      Cell 
        { nDays = 0
        , cellState = Healthy
        , cellRandomGenerator = mkStdGen genSeed
        }

    row :: State Int (ListZipper Cell)
    row = do
      left  <- replicateM rH genCell
      x     <- genCell
      right <- replicateM rH' genCell
      return $ LZ left x right
      where 
        genCell :: State Int Cell
        genCell = modify (+1) >> get >>= (return . rawCell)

    grid :: Grid Cell
    grid = flip evalState seed $ do
      left  <- replicateM rW row
      x     <- row
      right <- replicateM rW' row
      return $ 
        gridUpdate (\cell -> cell { cellState = Sick }) $ 
          Grid $ LZ left x right

update :: InfectionInfo -> Grid Cell -> Cell 
update infection grid = 
    case cellState cell of
      Healthy -> 
        let (p, newCell) = cellProbability cell 
            stayHealthyP = 
              infectionP infection * fromIntegral (infectedNeighbours grid) in
            if p >= stayHealthyP
               then updateCell newCell
               else newState Incubation newCell
      Incubation -> 
        if nDays cell == incubationPeriod infection 
        then newState Sick cell
        else updateCell cell
      Sick -> 
        if nDays cell == infectiousPeriod infection 
        then newState Immune cell
        else updateCell cell
      Immune -> 
        if nDays cell == immunityPeriod infection 
        then newState Healthy cell
        else updateCell cell
  where cell = extract grid

nextGrid :: CovidGrid -> CovidGrid
nextGrid (CovidGrid grid infection) = 
  CovidGrid (extend (update infection) grid) infection

showCovid :: Grid Cell -> Grid String
showCovid grid = show <$> grid 

instance Show Cell where
  show cell = 
    case cellState cell of
      Healthy    -> "_"
      Incubation -> "*"
      Sick       -> "#"
      Immune     -> "@"

instance Show CovidGrid where
  show (CovidGrid grid _) = showGrid $ showCovid grid
    where 
      showRow :: ListZipper String -> String
      showRow (LZ ls x rs) = reverse (concat ls) ++ x ++ concat rs ++ "\n"

      showGrid :: Grid String -> String
      showGrid (Grid (LZ ls x rs)) = concat (showRow <$> reverse ls ++ [x] ++ rs) 

