{-# LANGUAGE OverloadedStrings #-}

{- | pandas-style reshaping: @.T@, @stack@ and @unstack@. DataFrames here have
no index, so the key columns are passed explicitly.

> import qualified Reshape as R
>
> R.transpose "Indicator Code" df                        -- df.set_index(key).T
> long <- pure (R.stack ["Indicator Code"] "year" "value" df)   -- wide -> long
> R.unstack ["year"] "Indicator Code" "value" long       -- long -> wide

Columns are rebuilt from their values. Value columns (the data) are always
@Double@ when numeric; key columns and column names turned into rows (years)
become @Int@ when they are whole numbers; everything else is @Text@. A column
with missing values becomes @Maybe Double@ / @Maybe Int@ / @Maybe Text@.
-}
module Reshape (transpose, stack, unstack, columnFromCells) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import qualified DataFrame as D
import DataFrame.Internal.Column (Column)
import DataFrame.Internal.DataFrame (DataFrame, columnNames)
import DfCells (Cell (..), cells, isNumericCol)

{- | Swap rows and columns. The values of the @key@ column become the new
column names; the other column names go into a first column called
@"column"@ (like pandas @df.set_index(key).T@).
-}
transpose :: T.Text -> DataFrame -> DataFrame
transpose key df
    | dups /= [] = error $ "Reshape.transpose: repeated values in " ++ show key ++ ": " ++ show (take 5 dups)
    | otherwise =
        D.fromNamedColumns $
            ("column", columnFromCells (map parseCell others))
                : zip keys (map valueColumn (L.transpose rows))
  where
    keys = map cellText (cells df key)
    dups = map head (filter ((> 1) . length) (L.group (L.sort keys)))
    others = filter (/= key) (columnNames df)
    rows = map (cells df) others

{- | Wide to long (pandas @stack@ / @melt@): every column not in @ids@ becomes
rows, with its name in @varName@ and its value in @valueName@. Column names
that look like numbers (years) become a numeric column.

> R.stack ["Indicator Code"] "year" "value" df
-}
stack :: [T.Text] -> T.Text -> T.Text -> DataFrame -> DataFrame
stack ids varName valueName df =
    D.fromNamedColumns $
        [(i, columnFromCells (concat (replicate (length vars) (cells df i)))) | i <- ids]
            ++ [ (varName, columnFromCells (map parseCell (concatMap (replicate nRows) vars)))
               , (valueName, valueColumn (concatMap (cells df) vars))
               ]
  where
    vars = filter (`notElem` ids) (columnNames df)
    nRows = case columnNames df of
        (c : _) -> length (cells df c)
        [] -> 0

{- | Long to wide (pandas @unstack@ / @pivot@): one row per distinct @ids@
combination, one column per distinct value of @colName@, filled from
@valueName@. Missing combinations are null; repeated ones are an error.

> R.unstack ["year"] "Indicator Code" "value" long
-}
unstack :: [T.Text] -> T.Text -> T.Text -> DataFrame -> DataFrame
unstack ids colName valueName df
    | dups /= [] = error $ "Reshape.unstack: repeated rows for " ++ show (take 3 dups)
    | otherwise =
        D.fromNamedColumns $
            [(i, columnFromCells [k !! j | k <- rowKeys]) | (j, i) <- zip [0 ..] ids]
                ++ [ (c, valueColumn [M.findWithDefault CNull (map cellText k, c) table | k <- rowKeys])
                   | c <- newCols
                   ]
  where
    idCells = L.transpose (map (cells df) ids)
    keyTexts = map (map cellText) idCells
    colVals = map cellText (cells df colName)
    vals = cells df valueName
    entries = zip (zip keyTexts colVals) vals
    table = M.fromList entries
    dups = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [(k, 1) | (k, _) <- entries]))
    -- Row keys keep the original cells (so types survive), in first-seen order.
    rowKeys = nubOn (map cellText) idCells
    newCols = nubOn id colVals

nubOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOn f = go mempty
  where
    go _ [] = []
    go seen (x : xs)
        | M.member (f x) seen = go seen xs
        | otherwise = x : go (M.insert (f x) () seen) xs

-- | Like 'columnFromCells', but numeric values stay 'Double' even when they
-- are all whole numbers (GDP in constant LCU, counts, ...), so @log@ and
-- division work on them.
valueColumn :: [Cell] -> Column
valueColumn cs
    | isNumericCol cs && CNull `elem` cs = D.fromList [case c of CNum x -> Just x; _ -> Nothing | c <- cs]
    | isNumericCol cs = D.fromList [x | CNum x <- cs]
    | otherwise = columnFromCells cs

-- | Build a typed column from cells (see the module header for the rules).
columnFromCells :: [Cell] -> Column
columnFromCells cs
    | isNumericCol cs && integral = if hasNull then D.fromList (map (fmap round . num) cs :: [Maybe Int]) else D.fromList [round x :: Int | CNum x <- cs]
    | isNumericCol cs = if hasNull then D.fromList (map num cs) else D.fromList [x | CNum x <- cs]
    | hasNull = D.fromList (map txt cs)
    | otherwise = D.fromList [cellText c | c <- cs]
  where
    hasNull = CNull `elem` cs
    integral = and [x == fromInteger (round x) && abs x < 9e18 | CNum x <- cs]
    num (CNum x) = Just x
    num _ = Nothing
    txt CNull = Nothing
    txt c = Just (cellText c)

parseCell :: T.Text -> Cell
parseCell t = case reads (T.unpack t) :: [(Double, String)] of
    [(x, "")] -> CNum x
    _ -> CStr t

cellText :: Cell -> T.Text
cellText (CStr s) = s
cellText (CNum x)
    | x == fromInteger r = T.pack (show r)
    | otherwise = T.pack (show x)
  where
    r = round x :: Integer
cellText CNull = ""
