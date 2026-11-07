{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Readable DataFrame printing, pandas style.

> import Pretty (pp, ppWith, defaultPP, PPOpts (..))
>
> pp df                                       -- 20 rows, 2 decimals
> ppWith defaultPP{decimals = 4, rows = 40} df

* no @Just@ / @Nothing@: nulls print as @·@
* numbers right-aligned, rounded, with thousands separators; integer
  columns and values >= 100,000 print without decimals; tiny/huge values
  switch to scientific
* long tables show head and tail, wide tables first and last columns

@pp@ also prints hmatrix matrices and vectors numpy style (see "NpShow"):

> pp diff2      -- [[ 1. -2.  1. ...  0.  0.  0.]
>               --  ...
-}
module Pretty (Pretty (..), ppWith, PPOpts (..), defaultPP) where

import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric (showEFloat, showFFloat)
import Numeric.LinearAlgebra (Matrix, Vector)

import DataFrame.Internal.DataFrame (DataFrame, columnNames)
import DfCells (Cell (..), cells, isNumericCol)
import NpShow (showMatrix, showVector)

data PPOpts = PPOpts
    { rows :: Int
    -- ^ Rows shown before eliding the middle (head + tail).
    , columns :: Int
    -- ^ Columns shown before eliding the middle.
    , decimals :: Int
    -- ^ Decimals for non-integer numeric columns.
    , cellWidth :: Int
    -- ^ Text cells and headers longer than this are cut with an ellipsis.
    , index :: Bool
    -- ^ Show the row number column on the left.
    , width :: Int
    -- ^ Maximum line width; middle columns are dropped until the table fits.
    }

defaultPP :: PPOpts
defaultPP = PPOpts{rows = 20, columns = 12, decimals = 2, cellWidth = 28, index = True, width = 80}

class Pretty a where
    pp :: a -> IO ()

instance Pretty DataFrame where
    pp = ppWith defaultPP

instance Pretty (Matrix Double) where
    pp = putStrLn . showMatrix

instance Pretty (Vector Double) where
    pp = putStrLn . showVector

ppWith :: PPOpts -> DataFrame -> IO ()
ppWith o df = TIO.putStrLn (render o df)

data Align = L | R

-- | A rendered column: alignment, header, cells (Nothing = elided-row marker).
type Col = (Align, T.Text, [T.Text])

render :: PPOpts -> DataFrame -> T.Text
render o df
    | null names = "(empty DataFrame)"
    | otherwise = T.intercalate "\n" (header : rule : body ++ ["", footer])
  where
    names = columnNames df
    nRows = length (cells df (head names))
    nCols = length names

    -- Which rows/columns to show; Nothing marks the elided gap.
    rowSel = elide (rows o) [0 .. nRows - 1]
    colSel = fitColumns (width o) (columns o) indexW gapW (map (colWidth . dataCol) names) names

    cols = indexCol ++ map (maybe gapCol dataCol) colSel
    indexW = if index o then T.length (T.pack (show (nRows - 1))) + 2 else 0
    gapW = 3
    colWidth (_, h, cs) = maximum (T.length h : map T.length cs) + 2
    indexCol = [(R, "", map (maybe "⋮" (T.pack . show)) rowSel) | index o]
    gapCol = (L, "…", map (const "…") rowSel)

    dataCol name =
        let cs = cells df name
            numeric = isNumericCol cs
            fmt = if numeric then numFormatter (decimals o) cs else textCell
            shown = map (maybe "⋮" (fmt . (cs !!))) rowSel
         in (if numeric then R else L, clip (cellWidth o) name, shown)

    textCell (CStr s) = clip (cellWidth o) s
    textCell (CNum d) = T.pack (show d)
    textCell CNull = "·"

    widths = [maximum (T.length h : map T.length cs) | (_, h, cs) <- cols]
    pad (a, w) t = case a of
        L -> T.justifyLeft w ' ' t
        R -> T.justifyRight w ' ' t
    aligns = [a | (a, _, _) <- cols]
    line ts = T.stripEnd (T.intercalate "  " (zipWith pad (zip aligns widths) ts))
    header = line [h | (_, h, _) <- cols]
    rule = T.intercalate "  " [T.replicate w "─" | w <- widths]
    body = map line (transpose [cs | (_, _, cs) <- cols])
    footer = T.pack (show nRows) <> " rows × " <> T.pack (show nCols) <> " columns"

-- | Take columns alternately from the left and right edges while they fit
-- in the line width (and the column cap); a 'Nothing' marks the dropped
-- middle. Always keeps at least the first column.
fitColumns :: Int -> Int -> Int -> Int -> [Int] -> [a] -> [Maybe a]
fitColumns maxW maxN indexW gapW ws xs
    | n <= maxN && indexW + sum ws <= maxW + 2 = map Just xs
    | otherwise = map Just (take l xs) ++ [Nothing] ++ map Just (drop (n - r) xs)
  where
    n = length xs
    order = concat [[Left i, Right (n - 1 - i)] | i <- [0 .. n `div` 2]]
    sizes = [either (ws !!) (ws !!) k | k <- order]
    budget = maxW + 2 - indexW - gapW
    fits = length (takeWhile (<= budget) (scanl1 (+) sizes))
    k = max 1 (min (maxN - 1) fits)
    l = (k + 1) `div` 2
    r = k `div` 2

-- | Keep the first and last halves of a list when it is longer than n.
elide :: Int -> [a] -> [Maybe a]
elide n xs
    | n <= 0 || length xs <= n = map Just xs
    | otherwise = map Just (take h xs) ++ [Nothing] ++ map Just (drop (length xs - (n - h)) xs)
  where
    h = (n + 1) `div` 2

clip :: Int -> T.Text -> T.Text
clip w t
    | T.length t <= w = t
    | otherwise = T.take (w - 1) t <> "…"

-- | Integer columns print as integers; otherwise each value is formatted by
-- size: tiny or huge values in scientific notation, values >= 100,000
-- without decimals, the rest with @d@ decimals.
numFormatter :: Int -> [Cell] -> Cell -> T.Text
numFormatter d cs = fmt
  where
    integral = and [x == fromInteger (round x) && abs x < 1e15 | CNum x <- cs]
    fmt CNull = "·"
    fmt (CStr s) = s
    fmt (CNum x)
        | integral || x == 0 = grouped (T.pack (show (round x :: Integer)))
        | abs x < 10 ^^ negate d || abs x >= 1e15 = T.pack (showEFloat (Just d) x "")
        | abs x >= 1e5 = grouped (T.pack (show (round x :: Integer)))
        | otherwise =
            let (i, f) = T.breakOn "." (T.pack (showFFloat (Just d) x ""))
             in grouped i <> f

-- | Thousands separators from 10,000 up: "-1234567" -> "-1,234,567", but
-- "1960" stays "1960" (years).
grouped :: T.Text -> T.Text
grouped t
    | T.length digits <= 4 = t
    | otherwise = sign <> T.intercalate "," (reverse (map T.reverse (T.chunksOf 3 (T.reverse digits))))
  where
    (sign, digits) = T.span (== '-') t
