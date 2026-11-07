{- | numpy-style printing of hmatrix values (what @print(a)@ shows in numpy).

> putStrLn (showMatrix m)
> putStrLn (showVector v)

* whole numbers print as @1.@ / @-2.@; otherwise up to 8 decimals with
  trailing zeros trimmed (uniform across the array); very large/small
  ranges switch to scientific
* more than 1000 elements: only the first and last 3 rows/columns, with @...@
* lines wrap at 75 characters
-}
module NpShow (showMatrix, showVector) where

import Data.List (intercalate)
import Numeric (showEFloat, showFFloat)
import Numeric.LinearAlgebra (Matrix, Vector, cols, rows, toList, toLists)

threshold, edge, lineWidth :: Int
threshold = 1000
edge = 3
lineWidth = 75

showVector :: Vector Double -> String
showVector v = "[" ++ wrapRow 1 (cells fmt w xs) ++ "]"
  where
    xs = elide (length (toList v) > threshold) (toList v)
    shown = [x | Just x <- xs]
    fmt = formatter shown
    w = maximum (0 : map (length . fmt) shown)

showMatrix :: Matrix Double -> String
showMatrix m
    | rows m == 0 || cols m == 0 = "[]  (" ++ show (rows m) ++ "x" ++ show (cols m) ++ ")"
    | otherwise = "[" ++ intercalate "\n " (map row rs) ++ "]" ++ shape
  where
    summarize = rows m * cols m > threshold
    rs = elide summarize (map (elide summarize) (toLists m))
    shown = [x | Just r <- rs, Just x <- r]
    fmt = formatter shown
    w = maximum (0 : map (length . fmt) shown)
    row Nothing = " ..."
    row (Just r) = "[" ++ wrapRow 2 (cells fmt w r) ++ "]"
    shape = if summarize then "\n(" ++ show (rows m) ++ "x" ++ show (cols m) ++ ")" else ""

-- | Keep the first and last 'edge' items, Nothing marks the cut.
elide :: Bool -> [a] -> [Maybe a]
elide False xs = map Just xs
elide True xs
    | length xs <= 2 * edge = map Just xs
    | otherwise = map Just (take edge xs) ++ [Nothing] ++ map Just (drop (length xs - edge) xs)

-- | Format and right-align to width @w@; Nothing becomes @...@.
cells :: (Double -> String) -> Int -> [Maybe Double] -> [String]
cells fmt w = map (maybe "..." pad)
  where
    pad x = let s = fmt x in replicate (w - length s) ' ' ++ s

-- | Space-separated cells, wrapped at 'lineWidth' with @indent@ spaces.
wrapRow :: Int -> [String] -> String
wrapRow indent = intercalate ("\n" ++ replicate indent ' ') . go (lineWidth - indent - 1)
  where
    go _ [] = []
    go room (c : cs) = let (line, rest) = fill room [c] cs in unwords line : go room rest
    fill room acc (c : cs)
        | length (unwords (acc ++ [c])) <= room = fill room (acc ++ [c]) cs
    fill _ acc cs = (acc, cs)

-- | One number format for the whole array, like numpy's default.
formatter :: [Double] -> Double -> String
formatter xs
    | all integral finite = \x -> special x (show (round x :: Integer) ++ ".")
    | scientific = \x -> special x (sci x)
    | otherwise = \x -> special x (padRight (fixed x))
  where
    finite = filter (\x -> not (isNaN x || isInfinite x)) xs
    absNZ = filter (> 0) (map abs finite)
    scientific =
        not (null absNZ)
            && (maximum absNZ >= 1e8 || minimum absNZ < 1e-4 || maximum absNZ / minimum absNZ > 1e3)
    integral x = x == fromInteger (round x)
    -- fewest decimals (<= 8) that show every value exactly at that precision
    trim s = case break (== '.') s of
        (i, '.' : d) -> let d' = reverse (dropWhile (== '0') (reverse d)) in (i, d')
        (i, _) -> (i, "")
    fixedRaw x = trim (showFFloat (Just 8) x "")
    prec = maximum (0 : [length (snd (fixedRaw x)) | x <- finite])
    fixed x = let (i, d) = fixedRaw x in i ++ "." ++ d
    padRight s = let (_, d) = trim s in s ++ replicate (prec - length d) ' '
    sciPrec = maximum (0 : [length (sciDigits x) | x <- finite])
    sciDigits x = case break (== 'e') (showEFloat (Just 8) x "") of
        (mant, _) -> snd (trim mant)
    sci x = case break (== 'e') (showEFloat (Just sciPrec) x "") of
        (mant, 'e' : ex) -> mant ++ "e" ++ expo ex
        (mant, _) -> mant
    expo ('-' : e) = '-' : twoDigits e
    expo e = '+' : twoDigits e
    twoDigits e = replicate (2 - length e) '0' ++ e
    special x s
        | isNaN x = "nan"
        | isInfinite x = if x > 0 then "inf" else "-inf"
        | otherwise = s
