-- Exportar resultados para el documento LaTeX
import Text.Printf (printf)
import qualified Data.List as L
years = M.columnVector "year" df4
cols = [years, pbi, consumption, investment] ++ map flatten [pbi_trend_100, consumption_trend_100, investment_trend_100, pbi_cycles, consumption_cycles, investment_cycles]
hdr = "year,ln_y,ln_c,ln_i,trend_y,trend_c,trend_i,cycle_y,cycle_c,cycle_i"
writeFile "series_haskell.csv" $ unlines $ hdr : [L.intercalate "," (map (printf "%.15g") r) | r <- toLists (fromColumns cols)]
stats = [("y", pbi_std, 1, 1), ("c", consumption_std, consumption_std/pbi_std, corr_pbi_cons), ("i", investment_std, investment_std/pbi_std, corr_pbi_inv)] :: [(String, Double, Double, Double)]
writeFile "results_haskell.csv" $ unlines $ "variable,sd,rel_sd,corr_y" : [printf "%s,%.15g,%.15g,%.15g" k s r c | (k, s, r, c) <- stats]
