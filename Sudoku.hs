-- Name: Wei XING (Phillip)
-- UID: u5656487
-- Collaborators:  tutor:   Yiping Su (u5925716)
--                 student: Yian Hu (u5842618), Shukai Zhang (u5686922)
module Sudoku
  ( allBlanks
  , isSudoku
  , noBlanks
  , printSudoku
  , fromString
  , toString
  , rows
  , cols
  , boxs
  , okBlock
  , okSudoku
  , blank
  , (!!=)
  , update
  , solve
  ) where

import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck

-- A matrix is a list of rows.
type Matrix a = [Row a]

-- A row is a list of values
type Row a = [a]

-- A Sudoku puzzle is a matrix of cells
newtype Sudoku =
  Sudoku (Matrix Cell)
  deriving (Show, Eq)

-- | cells extracts the cells from a Sudoku
cells (Sudoku m) = m

-- Each cell may contain a number from 1 to 9, or nothing
type Cell = Maybe Int

example :: Sudoku
example =
  Sudoku
    [ [ Just 3
      , Just 6
      , Nothing
      , Nothing
      , Just 7
      , Just 1
      , Just 2
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 5
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 8
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 9
      , Just 2
      , Nothing
      , Just 4
      , Just 7
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 1
      , Just 3
      , Nothing
      , Just 2
      , Just 8
      ]
    , [ Just 4
      , Nothing
      , Nothing
      , Just 5
      , Nothing
      , Just 2
      , Nothing
      , Nothing
      , Just 9
      ]
    , [ Just 2
      , Just 7
      , Nothing
      , Just 4
      , Just 6
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 5
      , Just 3
      , Nothing
      , Just 8
      , Just 9
      , Nothing
      , Nothing
      ]
    , [ Nothing
      , Just 8
      , Just 3
      , Nothing
      , Nothing
      , Nothing
      , Nothing
      , Just 6
      , Nothing
      ]
    , [ Nothing
      , Nothing
      , Just 7
      , Just 6
      , Just 9
      , Nothing
      , Nothing
      , Just 4
      , Just 3
      ]
    ]

-- allBlanks is a Sudoku with just blanks
allBlanks :: Sudoku
allBlanks = Sudoku (replicate 9 (replicate 9 Nothing))

-- | isSudoku checks if a Sudoku has the proper dimensions
-- >>> isSudoku (Sudoku [])
-- False
-- >>> isSudoku allBlanks
-- True
-- >>> isSudoku example
-- True
-- >>> isSudoku (Sudoku (tail (cells example)))
-- False
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku list) = case length list of
    9           -> all (\x -> length x == 9) list
    _           -> False

-- | noBlanks checks if a Sudoku has no blanks

noBlanks :: Sudoku -> Bool
noBlanks x = Nothing `notElem` concat (cells x)

-- | printSudoku prints a Sudoku as a 9 x 9 grid
-- Example:
--    3 6 . . 7 1 2 . .
--    . 5 . . . . 1 8 .
--    . . 9 2 . 4 7 . .
--    . . . . 1 3 . 2 8
--    4 . . 5 . 2 . . 9
--    2 7 . 4 6 . . . .
--    . . 5 3 . 8 9 . .
--    . 8 3 . . . . 6 .
--    . . 7 6 9 . . 4 3


-- reference: https://stackoverflow.com/questions/41512822/error-when-changing-function-name-to-list-haskell

convertInt :: Cell -> Char
convertInt Nothing = '.'
convertInt (Just x) = chr (x+48)

printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku x) = putStr (unlines [ map convertInt y | y <- cells (Sudoku x)])


-- | cell generates an arbitrary cell in a Sudoku
-- The frequency of Nothing versus Just n values is currently 90% versus 10%,
-- but you may want to change that ratio.
cell :: Gen (Maybe Int)
cell =
  frequency
    [(10, oneof [return (Just n) | n <- [1 .. 9]]), (90, return Nothing)]


-- | An instance for generating Arbitrary Sudokus
-- prop> isSudoku s
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
    return (Sudoku rows)

-- | fromString converts an 81-character canonical string encoding for a
-- | Sudoku into our internal representation

-- >>> toString example
-- "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- >>> fromString (toString example)
-- Sudoku [[Just 3,Just 6,Nothing,Nothing,Just 7,Just 1,Just 2,Nothing,Nothing],[Nothing,Just 5,Nothing,Nothing,Nothing,Nothing,Just 1,Just 8,Nothing],[Nothing,Nothing,Just 9,Just 2,Nothing,Just 4,Just 7,Nothing,Nothing],
-- [Nothing,Nothing,Nothing,Nothing,Just 1,Just 3,Nothing,Just 2,Just 8],[Just 4,Nothing,Nothing,Just 5,Nothing,Just 2,Nothing,Nothing,Just 9],[Just 2,Just 7,Nothing,Just 4,Just 6,Nothing,Nothing,Nothing,Nothing],
-- [Nothing,Nothing,Just 5,Just 3,Nothing,Just 8,Just 9,Nothing,Nothing],[Nothing,Just 8,Just 3,Nothing,Nothing,Nothing,Nothing,Just 6,Nothing],[Nothing,Nothing,Just 7,Just 6,Just 9,Nothing,Nothing,Just 4,Just 3],[]]

fromString :: String -> Sudoku
fromString str = Sudoku (split 8 (map convertChar str))
   where
         convertChar :: Char -> Cell
         convertChar '.'      = Nothing
         convertChar x        = Just (digitToInt x)

         split :: Int -> [Cell] -> [[Cell]]
         split 1 xs = [take 9 xs, drop 9 xs]
         split n xs =  take 9 xs:(split (n-1) (drop 9 xs))

-- | toString converts a Sudoku into its canonical 81-character string
-- | encoding
-- prop> fromString (toString s) == s
toString :: Sudoku -> String
toString (Sudoku s) = map convertInt (concat s)

type Block a = [a]

-- >>> rows [[1,2,3,4,5],[5,6,7,8],[9,10,11,12,13]]
--[[1,2,3,4,5],[5,6,7,8],[9,10,11,12,13]]
rows :: Matrix a -> [Block a]
rows rs = rs

-- >>> cols [[1,2,3,4,5],[5,6,7,8],[9,10,11,12,13]]
-- [[1,5,9],[2,6,10],[3,7,11],[4,8,12],[5,13]]
cols :: Matrix a -> [Block a]
cols = transpose

-- https://stackoverflow.com/questions/31360775/find-9-3x3-blocks-of-sudoku-board-haskell

-- >>> boxs [[1,2,3,4,5],[5,6,7,8],[9,10,11,12,13], [14,15,16,17,18]]
-- [[1,2,3,5,6,7,9,10,11],[14,15,16,4,5,8],[12,13,17,18]]

boxs :: Matrix a -> [Block a]
boxs =  map concat . group3 . concat . transpose . map group3
  where
     group3 (a:b:c:ds) = [a,b,c] : group3 ds
     group3 []         = []
     group3 as         = [ as ]

-- | Test if a block of cells does not contain the same integer twice
-- >>> okBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
-- True
-- >>> okBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
-- False
okBlock :: Block Cell -> Bool
okBlock [ ]   = True
okBlock (x:xs) = case x of
     Nothing       -> okBlock xs
     Just int      -> notElem x xs && okBlock xs                                          -- Yian Hu hints me to use notElem function

-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True

okSudoku :: Sudoku -> Bool
okSudoku (Sudoku x) = all okBlock (rows x) && all okBlock (cols x) && all okBlock (boxs x)  -- tutor Yiping Su reminds me to use all function

type Pos = (Int, Int)

-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)
-- >>> blank example
-- (0,2)
blank :: Sudoku -> Pos
blank x = (number `div` 9, number `mod` 9)
  where
    number = helper $ elemIndex Nothing $ concat (cells x)
       where helper :: Maybe Int -> Int
             helper Nothing   = 0
             helper (Just x)  = x                                                           -- Shukai Zhang hints me to use eleIndex function

-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) a (int,b) = (take int a) ++ [b] ++ (drop (int+1) a)


-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.

-- >>> update example  (1,3) 3
-- Sudoku [[Just 3,Just 6,Nothing,Nothing,Just 7,Just 1,Just 2,Nothing,Nothing],[Nothing,Just 3,Nothing,Nothing,Nothing,Nothing,Just 1,Just 8,Nothing],[Nothing,Nothing,Just 9,Just 2,Nothing,Just 4,Just 7,Nothing,Nothing],
-- [Nothing,Nothing,Nothing,Nothing,Just 1,Just 3,Nothing,Just 2,Just 8],[Just 4,Nothing,Nothing,Just 5,Nothing,Just 2,Nothing,Nothing,Just 9],[Just 2,Just 7,Nothing,Just 4,Just 6,Nothing,Nothing,Nothing,Nothing],
-- [Nothing,Nothing,Just 5,Just 3,Nothing,Just 8,Just 9,Nothing,Nothing],[Nothing,Just 8,Just 3,Nothing,Nothing,Nothing,Nothing,Just 6,Nothing],[Nothing,Nothing,Just 7,Just 6,Just 9,Nothing,Nothing,Just 4,Just 3]]

-- https://stackoverflow.com/questions/41126330/writing-sudoku-in-haskell-find-possible-candidates-for-a-cell

convertInt' :: Int -> Cell
convertInt' 0  = Just 0
convertInt' x  = Just x

update :: Sudoku -> Pos -> Int -> Sudoku
update (Sudoku x) (p,i) y = Sudoku ((rows x) !!= (p,z))
     where z = (rows x) !! p !!= (i,(convertInt' y))

-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any

solve :: String -> [String]
solve x = solve2 (solve1 (fromString x))
   where
         solve2 :: Maybe Sudoku -> [String]
         solve2  Nothing    = []
         solve2  (Just x)   = [toString x]

solve1 :: Sudoku -> Maybe Sudoku
solve1 x
   | not (okSudoku x)   = Nothing
   | noBlanks x         = Just x
   | otherwise          = solve' [solve1 $ update x (blank x) c | c <- [1..9]]
    where
          solve' :: [Maybe a] -> Maybe a
          solve' []            = Nothing
          solve' (Nothing:xs) = solve' xs
          solve' (Just x:xs)   = Just x

-- solve easy.txt result:
-- real    0m47.212s
-- user    0m46.940s
-- sys     0m0.196s
-- can't solve the hard one quickly



--   After reading extension, try to follow the introduction of how to use propagation in StackOverflow,
--   it seems a little faster than above, but still can't "solve" 'hard.txt' quickly, maybe I didn't use "propagate" correctly.

-- Reference : https://github.com/Freezard/haskell-sudoku/blob/master/Sudoku.hs

solveNew :: String -> [String]
solveNew str = solve2 (solveX blank propagate (fromString str))
 where
   solve2 :: Maybe Sudoku -> [String]
   solve2  Nothing    = []
   solve2  (Just x)   = [toString x]

blocks :: Sudoku -> [Block Cell]
blocks (Sudoku s) = (rows s) ++ (cols s) ++ squares (Sudoku s)
  where
   squares s = [square (x,y) s | y <- [0..2], x <- [0..2]]

blanksInBlocks :: Sudoku -> [(Int,Int)]
blanksInBlocks s = [ (n, blanksInBlock b) | (b,n) <- zip (blocks s) [0..] ]

blanksInBlock :: [Cell]-> Int
blanksInBlock block = length (filter (== Nothing) block)

missingInBlockIndex b =
    snd $
    head $
    filter (\p -> fst p == Nothing) $
    zip b [0..8]

row :: Int -> Sudoku -> [Cell]
row x  (Sudoku s) = (rows s) !! x

square :: (Int, Int) -> Sudoku -> [Cell]
square (x,y) (Sudoku s) =
      concat
      $ [take 3 (drop (x*3) row) | row <- take 3 (drop (y*3) (rows s))]

column :: Int -> Sudoku -> [Cell]
column x (Sudoku s) = transpose (rows s) !! x


missingInBlock :: Block Cell -> Int
missingInBlock b = maxInBlock - (foldl (+) 0 (map (fromMaybe 0) b)) where
maxInBlock = foldl (+) 0 [1..9]

solveX :: (Sudoku -> Pos) -> (Sudoku -> Maybe Sudoku) -> Sudoku -> Maybe Sudoku
solveX blank' propagate' s
  | not (okSudoku s)        = Nothing
  | noBlanks s              = Just s
  | propagated /= Nothing   = solveX blank' propagate' (fromJust propagated)
  | otherwise               = listToMaybe solutions
    where
      propagated = propagate' s
      solutions = [ fromJust sol | n <- [1..9],
                          let sol = solveX blank' propagate' (update s (blank' s) n), sol /= Nothing]


propagate :: Sudoku -> Maybe Sudoku
propagate s | null availBlocks  = Nothing
            | otherwise = propagateBlock s (fst $ head availBlocks)
             where
    availBlocks = filter (\p -> snd p == 1) (blanksInBlocks s)
    propagateBlock s n | n < 9     = Just (propagateRow s n)
                       | n < 18    = Just (propagateColumn s (n-9))
                       | otherwise = Just (propagateSquare s (n-18)) where
        idx = missingInBlockIndex
        val = missingInBlock

        propagateRow s k    = update s (k,idx (row k s)) (val (row k s))
        propagateColumn s k = update s (idx (column k s),k) (val (column k s))
        propagateSquare s k = update s (y, x) (val sq) where
            sq = square (mod k 3, div k 3) s
            y  = 3*(div k 3) + (div (idx sq) 3)
            x  = 3*(mod k 3) + (mod (idx sq) 3)

-- solve easy.txt result:
-- real    0m38.540s
-- user    0m38.348s
-- sys     0m0.180s
-- still can't solve the hard one quickly