# urep — Hansen (1985) RBC model solved with Uhlig's method, in Haskell

Problem set 3, exercise 3 (Macroeconomics II, UTDT): divisible vs. indivisible labor
(Hansen 1985, "Indivisible Labor and the Business Cycle").

- `urep.hs` — the notebook (GHCi cells): log-linearized model, Uhlig's undetermined
  coefficients (matrix quadratic solved with QZ), impulse responses, unfiltered
  variances (Lyapunov), HP-filtered simulations and a replication of Hansen's Table 1
  (100 simulations × 115 quarters) for both economies.
- `MatQuad.hs` — QZ decomposition (LAPACK `dgges`) and the stable solvent of `A X² + B X + C = 0`.
- `HpFilter.hs` — HP filter, standard deviations and correlations.
- `PlotlyExpress.hs`, `DfMatrix.hs`, `DfCells.hs`, `Reshape.hs`, `Pretty.hs`, `NpShow.hs` —
  small helpers (plotly-express-like charts on top of `dataframe`, conversions to `hmatrix`).
- `charts/` — the charts produced by the notebook (Plotly HTML).
- `rbc.hs`, `rbc_code.hs`, `run.ghci`, `data.csv` — earlier RBC exercise (World Bank WDI data for Argentina).

## Notebook format

Cells start with a `-----…HS` line; the code is followed by a `-----…HF` line and the
cell's GHCi output. `[html] charts/by-id/<id>.html` lines in the output are the charts.

## Running

GHC 9.6 with `hmatrix`, `dataframe`, `aeson`, `text`, `vector`, `random`. From this folder,
start `ghci` and paste/run the cells in order (the first cell loads the helper modules).
