-- | HP filter and second moments. A series is a Matrix Double: an n×1 column
--   (like `reshape 1 v` in rbc.hs) or a 1×n row.
--   Same construction as in rbc.hs (second-difference matrix, trend = (I + λ D'D)⁻¹ x).
--
--   > cy = hpCycle 1600 y          -- cyclical component (n×1)
--   > stdDev cy
--   > correlation cy (hpCycle 1600 x)
module HpFilter
  ( hpMatrix, hpTrend, hpCycle
  , mean, stdDev, covariance, correlation
  ) where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra

-- | (I + λ D'D)⁻¹ for a series of length len, D = second differences ((len-2) × len).
hpMatrix :: Double -> Int -> Matrix Double
hpMatrix lambda len = inv (idn + scale lambda (tr diff2 <> diff2))
  where
    idn        = ident len
    minus      = -1 * subMatrix (1,0) (len-1,len) idn
    zeroes_row = konst 0 (1,len)
    diff       = idn + (minus === zeroes_row)                   -- first differences
    diff2      = subMatrix (0,0) (len-2,len) (diff <> diff)     -- second differences

-- | HP trend. Input: n×1 column (or 1×n row); output: n×1 column.
hpTrend :: Double -> Matrix Double -> Matrix Double
hpTrend lambda m = hpMatrix lambda (rows x) <> x
  where x = asColumn (flatten m)

-- | HP cyclical component: series minus trend. Output: n×1 column.
hpCycle :: Double -> Matrix Double -> Matrix Double
hpCycle lambda m = x - hpTrend lambda x
  where x = asColumn (flatten m)

-- The moments below take a series as a Matrix: an n×1 column (like
-- `reshape 1 v` in rbc.hs) or a 1×n row; it is flattened either way.

-- | Mean of a series.
mean :: Matrix Double -> Double
mean m = sumElements m / fromIntegral (size (flatten m))

-- | Sample covariance (divides by n-1, as in rbc.hs). Series are demeaned first.
covariance :: Matrix Double -> Matrix Double -> Double
covariance mx my = (dx <.> dy) / fromIntegral (size dx - 1)
  where
    dx = flatten mx - scalar (mean mx)
    dy = flatten my - scalar (mean my)

-- | Sample standard deviation (n-1).
stdDev :: Matrix Double -> Double
stdDev m = sqrt (covariance m m)

-- | Correlation between two series of the same length.
correlation :: Matrix Double -> Matrix Double -> Double
correlation mx my = covariance mx my / (stdDev mx * stdDev my)
