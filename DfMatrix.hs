{- | Convert between @dataframe@ DataFrames and @hmatrix@ matrices.

> import qualified DfMatrix as M
> import Numeric.LinearAlgebra
>
> let xs = M.toMatrixCols ["gdp", "infl"] df   -- rows = observations
>     y  = M.columnVector "consumption" df
>     beta = xs <\> y                            -- OLS

Integer and floating columns become 'Double'; nulls become NaN.
Non-numeric columns raise an error naming the column.
-}
module DfMatrix (
    toMatrix,
    toMatrixCols,
    columnVector,
    fromMatrix,
) where

import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import Numeric.LinearAlgebra (Matrix, Vector, fromColumns, toColumns)

import DataFrame.Internal.Column (toDoubleVector)
import qualified DataFrame.Internal.Column as Col
import DataFrame.Internal.DataFrame (DataFrame, columnNames, fromNamedColumns, getColumn)

-- | Every column of the frame, in column order (rows = observations).
toMatrix :: DataFrame -> Matrix Double
toMatrix df = toMatrixCols (columnNames df) df

-- | The named columns, in the given order (rows = observations).
toMatrixCols :: [T.Text] -> DataFrame -> Matrix Double
toMatrixCols names df = fromColumns [columnVector n df | n <- names]

-- | One column as an hmatrix vector.
columnVector :: T.Text -> DataFrame -> Vector Double
columnVector name df = case getColumn name df of
    Nothing -> error $ "DfMatrix: no column named " ++ show name
    Just col -> case toDoubleVector col of
        Left e -> error $ "DfMatrix: column " ++ show name ++ " is not numeric: " ++ show e
        Right v -> VG.convert v

-- | Build a DataFrame from a matrix, naming its columns.
fromMatrix :: [T.Text] -> Matrix Double -> DataFrame
fromMatrix names m
    | length names /= length cols =
        error $ "DfMatrix.fromMatrix: " ++ show (length names) ++ " names for " ++ show (length cols) ++ " columns"
    | otherwise = fromNamedColumns (zip names (map (Col.fromUnboxedVector . VG.convert) cols))
  where
    cols = toColumns m
