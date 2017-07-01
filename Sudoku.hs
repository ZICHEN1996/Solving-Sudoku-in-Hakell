-- Name: Zichen Zhang
-- UID: u6161816
-- Collaborators: None

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

import Test.QuickCheck
import Data.Char
import Data.List


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
allBlanks = Sudoku $ replicate 9 blankrow
    where blankrow = replicate 9 Nothing


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
isSudoku sudo = isNine (cells sudo) && (all isNine $ cells sudo)
    where isNine l = length l == 9

-- | noBlanks checks if a Sudoku has no blanks
noBlanks :: Sudoku -> Bool
noBlanks sudo = all noblankcell $ cells sudo
    where
        noblankcell = all notblank
        notblank cell = cell /= Nothing

testblank :: Bool
testblank = not (noBlanks example)

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
printSudoku :: Sudoku -> IO ()
printSudoku sudo = putStrLn convertedSudo
    where
        convertedSudo = concat $ map convertRow $ cells sudo
        convertRow r = concat (map convertCell r) ++ "\n"
        convertCell (Just n) = show n
        convertCell _ = "."
--show "\n" is just "\n" but putStrLn "\n" gives a new line
--also the use of "." is to suit the type signature of concat

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
fromString :: String -> Sudoku
fromString string = Sudoku $ helper string
    where
        helper [] = []
        helper str = makeRow (take 9 str) : helper (drop 9 str)
        makeRow = map convert
        convert '.' = Nothing
        convert '0' = Nothing
        convert n = Just $ digitToInt n
--big finding : $ cannot always replace ()!

-- | toString converts a Sudoku into its canonical 81-character string
-- | encoding
-- prop> fromString (toString s) == s
toString :: Sudoku -> String
toString sudo = map encode $ concat $ cells sudo
    where
        encode Nothing = '.'
        encode (Just n) = intToDigit n

testFromTo :: Bool
testFromTo = toString (fromString ".5..1..4.1.7...6.2...9.5...2.8.3.5.1.4..7..2.9.1.8.4.6...4.1...3.4...7.9.2..6..1.") == ".5..1..4.1.7...6.2...9.5...2.8.3.5.1.4..7..2.9.1.8.4.6...4.1...3.4...7.9.2..6..1."

type Block a = [a]

--extracts rows, columns and 3*3 boxes

rows :: Matrix a -> [Block a]
rows m = m

cols :: Matrix a -> [Block a]
cols = transpose

boxs :: Matrix a -> [Block a]
boxs = formNineBoxes
    where
        formNineBoxes [] = []
        formNineBoxes m = formThreeBoxes (take 3 m) ++ formNineBoxes (drop 3 m)
        formThreeBoxes [[],[],[]] = []
        formThreeBoxes rows = formBoxFromRows rows : formThreeBoxes (map (drop 3) rows)
        formBoxFromRows = concatMap (take 3)

-- | Check structure of a Sudoku: 9 rows, 9 columns, 9 boxes, each of
-- | exactly 9 cells
-- prop> prop_Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku m) = (isNine (rows m)) && (isNine (cols m)) && (isNine (boxs m)) && (all isNine (boxs m)) && (all isNine (cols m)) && (all isNine (rows m))
    where isNine n = length n == 9

-- | Test if a block of cells does not contain the same integer twice
-- >>> okBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
-- True
-- >>> okBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
-- False
okBlock :: Block Cell -> Bool
okBlock block = helper block []
    where
        helper [] _ = True
        helper (x:xs) acc
            | x == Nothing = helper xs acc
            | x `elem` acc = False
            | otherwise = helper xs (x:acc)

testOkBlock :: Bool
testOkBlock = all okBlock ((rows (cells example)) ++ (cols (cells example)) ++ (boxs (cells example)))

-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True
okSudoku :: Sudoku -> Bool
okSudoku (Sudoku m) = all okBlock ((rows m) ++ (cols m) ++ (boxs m))

testOkSudo :: Bool
testOkSudo = okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"

type Pos = (Int, Int)

-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)
-- >>> blank example
-- (0,2)

blank :: Sudoku -> Pos
blank (Sudoku sudo) = helper sudo (0,0)
    where
        helper m (r,9) = helper m (r+1,0)
        helper m (r,c)
            | (m !! r) !! c == Nothing = (r,c)
            | otherwise = helper m (r,c+1)
-- test run time:0.02s
-- | Original way of implement:
--blank sudo = helper string 0
--    where
--   helper (x:xs) n
--       | isBlank x = ( n `div` 9 , n `mod` 9 )
--       | otherwise = helper xs (n+1)
--    string = toString sudo
--    isBlank c = c == '.'

--test run time:0.14s (laggy)


-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) (c:cs) (n,x)
    | n == 0 = x : cs
    | otherwise = c : (!!=) cs (n-1,x)

testupdatelist :: Bool
testupdatelist = ([1..100] !!= (59,60)) == [1..100]

-- | Given a matrix, a position, and a new cell value,
-- | update the given matrix at the given position with the new value.
update :: [[([Int],Char)]] -> Pos -> Int -> [[([Int],Char)]]
update m (r,c) n = m !!= (r , updatedRow)
    where
        updatedRow = (m !! r) !!= (c , ([n],'y'))

testUpdate :: Bool
testUpdate = update originalMatrix (5,5) 3 == [[([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),
                                              ([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([
                                              1,2,3,4,5,6,7,8,9],'n')],[([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([
                                              1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,
                                              2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n')],[([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,
                                              2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,
                                              3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n')],[([1,2,3,4,5,6,7,8,9],'n'),([1,2,
                                              3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,
                                              4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n')],[([1,2,3,
                                              4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,
                                              5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,
                                              6,7,8,9],'n')],[([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,
                                              6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([3],'y'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,
                                              2,3,4,5,6,7,8,9],'n')],[([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,
                                              2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,
                                              3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n')],[([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,
                                              3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,
                                              4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n')],[([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,
                                              4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,
                                              5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n'),([1,2,3,4,5,6,7,8,9],'n')]]


-- a matrix whose entries are lists containing all the possible values 1 to 9
originalMatrix :: [[([Int],Char)]]
originalMatrix = replicate 9 $ replicate 9 ([1..9],'n')


--a function to 1.assign a new value to an entry of our possibility matrix and 2. to make changes on the related blocks
assign :: [[([Int],Char)]] -> Pos -> Int -> [[([Int],Char)]]
assign m (r,c) n = if null (checkNewSingles newMatrix)
                   then newMatrix
                   else reassign (checkNewSingles newMatrix)
    where
        newMatrix = update (eliminate m (r,c) n) (r,c) n
        reassign [((row,col),k)] = assign newMatrix (row,col) k

--a function to eliminate possibilities of values for entries lie in the same 'neighbourhood'
eliminate :: [[([Int],Char)]] -> Pos -> Int -> [[([Int],Char)]]
eliminate m (r,c) n = boxEliminate
    where
        colEliminated = map eliminateCth m
        eliminateCth row = row !!= (c,eliminateEntry (originalCth row))
        originalCth row = row !! c
        eliminatedRow = map eliminateEntry originalRow
        originalRow = m !! r
        eliminateEntry (list,indic) = (delete n list,indic)
        boxEliminate = helper (colEliminated !!= (r,eliminatedRow)) (0,0)
        helper matrix (row,col)
            | (row >= (r `div` 3) * 3) && (row <= (r `div` 3) * 3 + 2) && (col >= (c `div` 3) * 3) && (col <= (c `div` 3) * 3 + 2) = helper (matrix !!= (row,((matrix !! row) !!= (col,(eliminateEntry ((matrix !! row) !! col)))))) (row,col+1)
            | row == 9 = matrix
            | col == 9 = helper matrix (row+1,0)
            | otherwise = helper matrix (row,col+1)

--check whether our changes on the matrix form any new confirmed value
checkNewSingles :: [[([Int],Char)]] -> [((Int,Int),Int)]
checkNewSingles m = helper (0,0)
    where
        readNum ([n],_) = n
        isNewOne (l,indi) = length l == 1 && indi == 'n'
        helper (9,_) = []
        helper (row,9) = helper (row+1,0)
        helper (row,col)
            | isNewOne ((m !! row) !! col) = [((row,col),readNum ((m !! row) !! col))]
            | otherwise = helper (row,col+1)


--convert a string to a matrix. As we add the entries, the possibility of values on the other entries also change
stringToMatrix :: String -> [[([Int],Char)]]
stringToMatrix string = helper originalMatrix (0,0) string
    where
        helper m (9,_) _ = m
        helper m _ [] = m
        helper m (row,9) l = helper m (row+1,0) l
        helper m (row,col) (x:xs)
            | x == '.' || x == '0' = helper m (row,col+1) xs
            | otherwise = helper (assign m (row,col) (digitToInt x)) (row,col+1) xs

--check whether the matrix is complete e.g. solution
checkComplete :: [[([Int],Char)]] -> Bool
checkComplete = all (all confirmed)
    where confirmed (_,indi) = indi == 'y'

--find the entry with least possible choices of values
findLeast :: [[([Int],Char)]] -> ([Int],(Int,Int))
findLeast m = helper (0,0) ([1..10],(0,0))
    where
        possibilities (p,_) = p
        helper (9,_) acc = acc
        helper (row,9) acc = helper (row+1,0) acc
        helper (row,col) acc
            | length (possibilities ((m !! row) !! col)) < length (possibilities acc) && length (possibilities ((m !! row) !! col)) > 1 = helper (row,col+1) ((possibilities ((m !! row) !! col)),(row,col))
            | otherwise = helper (row,col+1) acc

--to check whether the matrix is valid (no same values on the same blocks)
matrixIsValid :: [[([Int],Char)]] -> Bool
matrixIsValid = all (all notNull)
    where notNull (l,_) = not (null l)

--search the solutions for the matrix by brutal trying
search :: [[([Int],Char)]] -> ([Int],(Int,Int)) -> [String]
search _ ([],_) = []
search m ((x:xs),coord)
    | matrixIsValid (assign m coord x) = (if checkComplete (assign m coord x) then [matrixToString (assign m coord x)] else search (assign m coord x) (findLeast (assign m coord x))) ++ (search m (xs,coord))
    | otherwise = search m (xs,coord)

--convert a matrix to a string
matrixToString :: [[([Int],Char)]] -> String
matrixToString = concatMap (concatMap convert)
    where
        convert ([n],_) = [intToDigit n]

-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
solve :: String -> [String]
solve string
    | checkComplete (stringToMatrix string) = [matrixToString (stringToMatrix string)]
    | otherwise = search (stringToMatrix string) (findLeast (stringToMatrix string))

--I got the solution from an online calculator
test1 = solve "..3.2.6..9..3.5..1..18.64....81.29..7.......8..67.82....26.95..8..2.3..9..5.1.3.." == ["483921657967345821251876493548132976729564138136798245372689514814253769695417382"]
