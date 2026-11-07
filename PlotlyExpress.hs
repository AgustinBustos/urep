{-# LANGUAGE OverloadedStrings #-}

{- | A small plotly-express look-alike on top of the @dataframe@ library.

> import qualified DataFrame as D
> import qualified PlotlyExpress as Px
>
> main = do
>   df <- D.readCsv "data.csv"
>   Px.show $ Px.line df [Px.x "year", Px.y "gdp", Px.color "country", Px.title "GDP"]

Options mirror plotly express keyword arguments. 'show' writes the figure to
./charts/ (next to the notebook) and prints the @[html]@ marker so it renders inline
under the notebook cell; 'writeHtml' saves a standalone page.
-}
module PlotlyExpress (
    -- * Figures
    Figure (..),
    scatter,
    line,
    area,
    bar,
    histogram,
    box,
    pie,

    -- * Options (plotly express kwargs)
    Opt,
    x,
    y,
    ys,
    color,
    names,
    values,
    title,
    labels,
    markers,
    logX,
    logY,
    nbins,
    histnorm,
    barmode,
    horizontal,
    width,
    height,
    text,
    opacity,

    -- * Output and tweaks
    show,
    toHtml,
    writeHtml,
    updateLayout,
    updateTraces,
    addTrace,
) where

import Prelude hiding (show)
import qualified Prelude

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isAlphaNum, toLower)
import Data.List (intersperse, nub)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import qualified Data.Vector as VB
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, renameFile)

import DataFrame.Internal.DataFrame (DataFrame)
import DfCells (Cell (..), cells)

-- ---------------------------------------------------------------------------
-- Figure

data Figure = Figure {traces :: [Value], layout :: Value}

instance A.ToJSON Figure where
    toJSON (Figure ts l) = object ["data" .= ts, "layout" .= l]

-- ---------------------------------------------------------------------------
-- Options

data Opts = Opts
    { oX :: Maybe T.Text
    , oY :: [T.Text]
    , oColor :: Maybe T.Text
    , oNames :: Maybe T.Text
    , oValues :: Maybe T.Text
    , oText :: Maybe T.Text
    , oTitle :: Maybe T.Text
    , oLabels :: [(T.Text, T.Text)]
    , oMarkers :: Bool
    , oLogX :: Bool
    , oLogY :: Bool
    , oNbins :: Maybe Int
    , oHistnorm :: Maybe T.Text
    , oBarmode :: Maybe T.Text
    , oHorizontal :: Bool
    , oWidth :: Maybe Int
    , oHeight :: Maybe Int
    , oOpacity :: Maybe Double
    }

type Opt = Opts -> Opts

defaults :: Opts
defaults =
    Opts Nothing [] Nothing Nothing Nothing Nothing Nothing [] False False False
        Nothing Nothing Nothing False Nothing Nothing Nothing

x, color, names, values, text, title, histnorm, barmode :: T.Text -> Opt
x c o = o{oX = Just c}
color c o = o{oColor = Just c}
names c o = o{oNames = Just c}
values c o = o{oValues = Just c}
text c o = o{oText = Just c}
title t o = o{oTitle = Just t}
-- | @"percent"@, @"probability"@, @"density"@ or @"probability density"@.
histnorm t o = o{oHistnorm = Just t}
-- | @"group"@, @"stack"@, @"relative"@ or @"overlay"@.
barmode t o = o{oBarmode = Just t}

-- | One y column.
y :: T.Text -> Opt
y c o = o{oY = oY o ++ [c]}

-- | Several y columns (wide form): one trace per column.
ys :: [T.Text] -> Opt
ys cs o = o{oY = oY o ++ cs}

-- | Rename columns in axis titles and legends, like @labels={...}@.
labels :: [(T.Text, T.Text)] -> Opt
labels ls o = o{oLabels = oLabels o ++ ls}

markers, logX, logY, horizontal :: Opt
markers o = o{oMarkers = True}
logX o = o{oLogX = True}
logY o = o{oLogY = True}
-- | @orientation="h"@: swaps the roles of x and y in bar/box/histogram.
horizontal o = o{oHorizontal = True}

nbins, width, height :: Int -> Opt
nbins n o = o{oNbins = Just n}
width n o = o{oWidth = Just n}
height n o = o{oHeight = Just n}

opacity :: Double -> Opt
opacity a o = o{oOpacity = Just a}

opts :: [Opt] -> Opts
opts = foldl (flip ($)) defaults

-- ---------------------------------------------------------------------------
-- Column extraction

cellJSON :: Cell -> Value
cellJSON (CNum d) = A.toJSON d
cellJSON (CStr s) = String s
cellJSON CNull = Null

cellText :: Cell -> T.Text
cellText (CNum d) =
    let r = round d :: Integer
     in if fromInteger r == d then T.pack (Prelude.show r) else T.pack (Prelude.show d)
cellText (CStr s) = s
cellText CNull = "null"

-- ---------------------------------------------------------------------------
-- Building traces

label :: Opts -> T.Text -> T.Text
label o c = fromMaybe c (lookup c (oLabels o))

-- | Split row indices by the color column (in order of first appearance).
groups :: DataFrame -> Opts -> [(Maybe T.Text, [Int])]
groups df o = case oColor o of
    Nothing -> [(Nothing, [0 .. nRows - 1])]
    Just c ->
        let ks = map cellText (cells df c)
         in [(Just k, [i | (i, k') <- zip [0 ..] ks, k' == k]) | k <- nub ks]
  where
    nRows = case oY o ++ maybe [] pure (oX o) ++ maybe [] pure (oValues o) of
        (c : _) -> length (cells df c)
        [] -> error "PlotlyExpress: specify at least x or y"

pickFast :: [Int] -> [Cell] -> Value
pickFast is cs = let v = VB.fromList cs in A.toJSON [cellJSON (v VB.! i) | i <- is]

-- | One trace per (y column, color group).
xyTraces :: DataFrame -> Opts -> [(K.Key, Value)] -> [Value]
xyTraces df o base =
    [ object $
        base
            ++ [ "x" .= maybe (A.toJSON [0 .. length is - 1]) (\c -> pickFast is (cells df c)) xcol
               , "y" .= pickFast is (cells df ycol)
               , "name" .= nm
               , "legendgroup" .= nm
               , "showlegend" .= (isJust g || length (oY o) > 1)
               ]
            ++ [k .= v | (k, Just v) <- [("text", fmap (\c -> pickFast is (cells df c)) (oText o))]]
            ++ ["opacity" .= a | Just a <- [oOpacity o]]
    | ycol <- oY o
    , (g, is) <- groups df o
    , let nm = case (g, oY o) of
            (Just k, [_]) -> k
            (Just k, _) -> label o ycol <> ", " <> k
            (Nothing, _) -> label o ycol
    ]
  where
    xcol = oX o

swapXY :: Opts -> Opts
swapXY o = case (oX o, oY o) of
    (Just xc, [yc]) -> o{oX = Just yc, oY = [xc]}
    _ -> o

baseLayout :: Opts -> [(T.Text, Value)]
baseLayout o =
    [ ("title", object ["text" .= t]) | Just t <- [oTitle o]
    ]
        ++ [("xaxis", axis (maybe "" (label o) (oX o)) (oLogX o))]
        ++ [("yaxis", axis yTitle (oLogY o))]
        ++ [("legend", object ["title" .= object ["text" .= label o c], "tracegrouporder" .= ("normal" :: T.Text)]) | Just c <- [oColor o]]
        ++ [("margin", object ["t" .= (if isJust (oTitle o) then 60 else 30 :: Int), "b" .= (60 :: Int), "l" .= (60 :: Int), "r" .= (30 :: Int)])]
        ++ [("width", A.toJSON w) | Just w <- [oWidth o]]
        ++ [("height", A.toJSON h) | Just h <- [oHeight o]]
        ++ [("barmode", String b) | Just b <- [oBarmode o]]
        ++ [("template", pxTemplate)]
  where
    yTitle = case oY o of
        [c] -> label o c
        [] -> ""
        _ -> "value"
    axis t lg = object $ ["title" .= object ["text" .= t]] ++ ["type" .= ("log" :: T.Text) | lg]

mkLayout :: [(T.Text, Value)] -> Value
mkLayout kvs = Object (KM.fromList [(K.fromText k, v) | (k, v) <- kvs])

figure :: Opts -> [Value] -> [(T.Text, Value)] -> Figure
figure o ts extra = Figure ts (mkLayout (baseLayout o ++ extra))

-- ---------------------------------------------------------------------------
-- Plot kinds

scatter :: DataFrame -> [Opt] -> Figure
scatter df os =
    let o = opts os
     in figure o (xyTraces df o [("type", "scatter"), ("mode", "markers")]) []

line :: DataFrame -> [Opt] -> Figure
line df os =
    let o = opts os
        mode = if oMarkers o then "lines+markers" else "lines" :: T.Text
     in figure o (xyTraces df o [("type", "scatter"), ("mode", String mode)]) []

-- | Stacked area chart (px.area).
area :: DataFrame -> [Opt] -> Figure
area df os =
    let o = opts os
     in figure o (xyTraces df o [("type", "scatter"), ("mode", "lines"), ("stackgroup", "one")]) []

bar :: DataFrame -> [Opt] -> Figure
bar df os =
    let o0 = opts os
        o = if oHorizontal o0 then swapXY o0 else o0
        ts = xyTraces df o [("type", "bar")]
        ts' = if oHorizontal o0 then map (orient . swapTrace) ts else ts
     in figure o0 ts' [("barmode", String (fromMaybe "relative" (oBarmode o0)))]

histogram :: DataFrame -> [Opt] -> Figure
histogram df os =
    let o = opts os
        col = fromMaybe (error "PlotlyExpress.histogram: needs x (or y with horizontal)") (oX o)
        axisKey = if oHorizontal o then "y" else "x"
        ts =
            [ object $
                [ "type" .= ("histogram" :: T.Text)
                , K.fromText axisKey .= pickFast is (cells df col)
                , "name" .= fromMaybe (label o col) g
                , "showlegend" .= isJust g
                ]
                    ++ ["nbinsx" .= n | Just n <- [oNbins o], not (oHorizontal o)]
                    ++ ["nbinsy" .= n | Just n <- [oNbins o], oHorizontal o]
                    ++ ["histnorm" .= h | Just h <- [oHistnorm o]]
                    ++ ["opacity" .= a | Just a <- [oOpacity o]]
            | (g, is) <- groups df o{oY = [col]}
            ]
        countLabel = fromMaybe "count" (oHistnorm o)
        o' = if oHorizontal o then o{oX = Just countLabel, oY = [label o col]} else o{oY = [countLabel]}
     in figure o' ts [("barmode", String (fromMaybe "relative" (oBarmode o)))]

-- | Box plot: y is the value column, x (optional) the grouping column.
box :: DataFrame -> [Opt] -> Figure
box df os =
    let o0 = opts os
        o = o0{oY = take 1 (oY o0)}
        ts = xyTraces df o [("type", "box")]
        ts' = if oHorizontal o0 then map (orient . swapTrace) ts else ts
     in figure o ts' [("boxmode", "group") | isJust (oColor o)]

pie :: DataFrame -> [Opt] -> Figure
pie df os =
    let o = opts os
        n = fromMaybe (error "PlotlyExpress.pie: needs names") (oNames o)
        v = fromMaybe (error "PlotlyExpress.pie: needs values") (oValues o)
        tr =
            object
                [ "type" .= ("pie" :: T.Text)
                , "labels" .= map cellJSON (cells df n)
                , "values" .= map cellJSON (cells df v)
                ]
     in Figure [tr] (mkLayout ([("title", object ["text" .= t]) | Just t <- [oTitle o]] ++ [("template", pxTemplate)]))

swapTrace :: Value -> Value
swapTrace (Object km) =
    Object $ KM.fromList [(sw k, v) | (k, v) <- KM.toList km]
  where
    sw k
        | k == "x" = "y"
        | k == "y" = "x"
        | otherwise = k
swapTrace v = v

orient :: Value -> Value
orient (Object km) = Object (KM.insert "orientation" (String "h") km)
orient v = v

-- ---------------------------------------------------------------------------
-- Tweaks

-- | Deep-merge a JSON object into the layout, like @fig.update_layout(...)@.
--
-- > Px.updateLayout (object ["yaxis" .= object ["tickformat" .= (".1%" :: Text)]]) fig
updateLayout :: Value -> Figure -> Figure
updateLayout v f = f{layout = merge (layout f) v}

-- | Deep-merge a JSON object into every trace, like @fig.update_traces(...)@.
updateTraces :: Value -> Figure -> Figure
updateTraces v f = f{traces = map (`merge` v) (traces f)}

-- | Append a raw plotly.js trace, like @fig.add_trace(...)@.
addTrace :: Value -> Figure -> Figure
addTrace t f = f{traces = traces f ++ [t]}

merge :: Value -> Value -> Value
merge (Object a) (Object b) = Object (KM.unionWith merge a b)
merge _ b = b

-- ---------------------------------------------------------------------------
-- Output

plotlyCdn :: T.Text
plotlyCdn = "https://cdn.plot.ly/plotly-2.35.2.min.js"

-- | Academic look: white background, black boxed axes with outside ticks,
--   faint grid, serif (LaTeX-like) font, muted print-friendly colors.
pxTemplate :: Value
pxTemplate =
    object
        [ "layout"
            .= object
                [ "plot_bgcolor" .= ("white" :: T.Text)
                , "paper_bgcolor" .= ("white" :: T.Text)
                , "font"
                    .= object
                        [ "family" .= ("Latin Modern Roman, CMU Serif, Computer Modern, Times New Roman, serif" :: T.Text)
                        , "size" .= (14 :: Int)
                        , "color" .= ("black" :: T.Text)
                        ]
                , "title" .= object ["x" .= (0.5 :: Double), "xanchor" .= ("center" :: T.Text)]
                , "xaxis" .= academicAxis
                , "yaxis" .= academicAxis
                , "legend"
                    .= object
                        [ "bgcolor" .= ("rgba(255,255,255,0.8)" :: T.Text)
                        , "bordercolor" .= ("black" :: T.Text)
                        , "borderwidth" .= (0.5 :: Double)
                        ]
                -- Paul Tol "muted" palette (colorblind-safe, made for scientific figures),
                -- ordered so the first few series contrast strongly
                , "colorway"
                    .= ( [ "#332288", "#CC6677", "#44AA99", "#DDCC77", "#882255"
                         , "#88CCEE", "#117733", "#999933", "#AA4499"
                         ] :: [T.Text]
                       )
                ]
        , "data"
            .= object
                [ "scatter" .= [object ["line" .= object ["width" .= (1.6 :: Double)]]]
                ]
        ]
  where
    academicAxis =
        object
            [ "showline" .= True
            , "linecolor" .= ("black" :: T.Text)
            , "linewidth" .= (1 :: Int)
            , "mirror" .= True
            , "ticks" .= ("outside" :: T.Text)
            , "tickcolor" .= ("black" :: T.Text)
            , "ticklen" .= (5 :: Int)
            , "showgrid" .= True
            , "gridcolor" .= ("#e6e6e6" :: T.Text)
            , "gridwidth" .= (0.5 :: Double)
            , "zeroline" .= True
            , "zerolinecolor" .= ("#999999" :: T.Text)
            , "zerolinewidth" .= (1 :: Int)
            , "automargin" .= True
            ]

-- | A @<div>@ plus the script that draws the figure into it (plotly.js
-- must already be loaded on the page). The toolbar is hidden in the
-- notebook's headless screenshot and shown in a real browser.
toHtml :: T.Text -> Figure -> T.Text
toHtml divId fig =
    T.unlines
        [ "<div id=\"" <> divId <> "\" style=\"width:100vw;height:100vh\"></div>"
        , "<script>if (/HeadlessChrome/.test(navigator.userAgent)) document.getElementById("
            <> T.pack (Prelude.show divId) <> ").style.height = window.outerHeight + 'px';</script>"
        , "<script>Plotly.newPlot(" <> T.pack (Prelude.show divId) <> ", "
            <> T.pack (BL.unpack (A.encode (traces fig))) <> ", "
            <> T.pack (BL.unpack (A.encode (layout fig))) <> ", {responsive: true, displayModeBar: !/HeadlessChrome/.test(navigator.userAgent)});</script>"
        ]

page :: Figure -> T.Text
page fig =
    "<!doctype html><html><head><meta charset=\"utf-8\">\n<script src=\""
        <> plotlyCdn
        <> "\" charset=\"utf-8\"></script></head><body style=\"margin:0\">\n"
        <> toHtml "plot" fig
        <> "</body></html>\n"

-- | Show the figure inline under the notebook cell (hasknote's @[html]@ marker).
--   Everything is saved next to the notebook (GHCi's working directory):
--
--   * ./charts/by-id/<id>.html  -- one file per run, never overwritten; the
--     notebook's marker points here by a short RELATIVE path
--     ("[html] charts/by-id/<id>.html <stamp>"), so it fits on one terminal
--     line and the notebook only references files inside the project.
--   * ./charts/<title>__<series>.html (see 'chartName') -- the latest version
--     under a readable name. If it already exists (you re-ran the cell), the
--     old one is first moved to ./charts/archive/<name>__<YYYYmmdd-HHMMSS>.html.
show :: Figure -> IO ()
show fig = do
    cwd <- getCurrentDirectory
    t <- getPOSIXTime
    let dir = cwd ++ "/charts"
        archive = dir ++ "/archive"
        byId = "charts/by-id"
        chartId = Prelude.show (round (t * 1e6) :: Integer)
        idPath = byId ++ "/" ++ chartId ++ ".html"
        name = chartName fig
        path = dir ++ "/" ++ name ++ ".html"
        html = page fig
    createDirectoryIfMissing True archive
    createDirectoryIfMissing True (cwd ++ "/" ++ byId)
    exists <- doesFileExist path
    if exists
        then do
            stamp <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
            renameFile path (archive ++ "/" ++ name ++ "__" ++ stamp ++ ".html")
        else pure ()
    TIO.writeFile path html
    TIO.writeFile (cwd ++ "/" ++ idPath) html
    putStrLn ("[html] " ++ idPath ++ " " ++ Prelude.show (round (t * 1000) :: Integer))

-- | File name for a figure: slug of the title plus the trace (series) names,
--   e.g. "ciclos-hp-lambda-1600__y_cycle-c_cycle-i_cycle". Falls back to the
--   axis titles, then to "chart".
chartName :: Figure -> String
chartName fig = case filter (not . null) [slug titleTxt, slug (T.intercalate "-" series)] of
    [] -> "chart"
    parts -> take 120 (concatMap id (intersperse "__" parts))
  where
    lay = layout fig
    titleTxt = textAt ["title", "text"] lay
    traceNames = nub (filter (not . T.null) [textAt ["name"] tr | tr <- traces fig])
    series
        | not (null traceNames) = traceNames
        | otherwise = filter (not . T.null) [textAt [ax, "title", "text"] lay | ax <- ["xaxis", "yaxis"]]
    textAt [] (String s) = s
    textAt (k : ks) (Object o) = maybe "" (textAt ks) (KM.lookup (K.fromText k) o)
    textAt _ _ = ""
    slug t =
        let s = map (\ch -> if isAlphaNum ch || ch == '_' then toLower ch else '-') (T.unpack t)
            collapse ('-' : '-' : r) = collapse ('-' : r)
            collapse (ch : r) = ch : collapse r
            collapse [] = []
         in dropWhile (== '-') (reverse (dropWhile (== '-') (reverse (collapse s))))

-- | Write a standalone HTML page.
writeHtml :: FilePath -> Figure -> IO ()
writeHtml path fig = TIO.writeFile path (page fig)
