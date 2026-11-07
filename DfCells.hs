{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Read DataFrame columns as plain numbers/strings, whatever their element
-- type (Int, Double, Maybe Double, Text, Day, ...). Shared by PlotlyExpress
-- and Pretty.
module DfCells (Cell (..), cells, isNumericCol) where

import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Data.Type.Equality (testEquality, (:~:) (Refl))
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as VU
import Type.Reflection (typeRep)

import DataFrame.Internal.Column (Bitmap, Column (..), bitmapTestBit)
import DataFrame.Internal.DataFrame (DataFrame, getColumn)

data Cell = CNum Double | CStr T.Text | CNull deriving (Eq)

-- | True when the column came out numeric (all cells 'CNum' or 'CNull').
isNumericCol :: [Cell] -> Bool
isNumericCol = all (\c -> case c of CStr _ -> False; _ -> True)

-- | Read a column as cells. Columns whose non-null values all parse as
-- numbers become numeric; everything else (dates, text, ...) becomes strings.
cells :: DataFrame -> T.Text -> [Cell]
cells df name = case getColumn name df of
    Nothing -> error $ "no column named " ++ show name
    Just col ->
        let raw = rawCells col
            nums = map toNum raw
         in if all isJust nums then map (fromMaybe CNull) nums else map (maybe CNull CStr) raw
  where
    toNum Nothing = Just CNull
    toNum (Just s) = case reads (T.unpack s) :: [(Double, String)] of
        [(d, "")]
            | isNaN d || isInfinite d -> Just CNull
            | otherwise -> Just (CNum d)
        _ -> Nothing

rawCells :: Column -> [Maybe T.Text]
rawCells (BoxedColumn bm (vec :: VB.Vector a)) =
    masked bm $ case testEquality (typeRep @a) (typeRep @T.Text) of
        Just Refl -> map Just (VB.toList vec)
        Nothing -> map (fromShown . show) (VB.toList vec)
rawCells (UnboxedColumn bm vec) = masked bm (map (fromShown . show) (VU.toList vec))

-- | Blank out rows marked invalid in the column's null bitmap.
masked :: Maybe Bitmap -> [Maybe T.Text] -> [Maybe T.Text]
masked Nothing xs = xs
masked (Just bm) xs = [if bitmapTestBit bm i then v else Nothing | (i, v) <- zip [0 ..] xs]

-- | Undo 'show' for the common wrappers (Maybe, quoted strings).
fromShown :: String -> Maybe T.Text
fromShown "Nothing" = Nothing
fromShown s
    | Just rest <- T.stripPrefix "Just " t = fromShown (T.unpack (unparen rest))
    | ('"' : _) <- s, [(str, "")] <- reads s = Just (T.pack str)
    | otherwise = Just t
  where
    t = T.pack s
    unparen u
        | T.isPrefixOf "(" u && T.isSuffixOf ")" u = T.init (T.tail u)
        | otherwise = u

