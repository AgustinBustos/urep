:set -XOverloadedStrings
:add PlotlyExpress.hs DfMatrix.hs Pretty.hs Reshape.hs
import qualified PlotlyExpress as Px
import qualified DfMatrix as M
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Pretty
import qualified Reshape as R
import Numeric.LinearAlgebra 
import qualified Numeric.LinearAlgebra as LA

df <- D.readCsv "data.csv"

-- D.columnAsList (F.col @T.Text "Country Name") df
-- D.columnNames df
-- ppWith defaultPP{rows = 40, decimals = 4} df
df1=D.exclude ["Country Code", "Country Name","Indicator Name" ] df
df2=R.transpose "Indicator Code" df1
df3=D.rename "column" "year" df2

-- pp $ df3 
Px.show $ Px.line df3 [Px.x "year", Px.y "NY.GDP.MKTP.KN" ]

:declareColumns df3

-- NY.GDP.MKTP.KN --  PIB, ny_gdp_mktp_kn 
-- NE.CON.TOTL.KN -- Consumo (p´ublico y privado), ne_con_totl_kn -- Consumo (p´ublico y privado)
-- NE.GDI.TOTL.KN. -- Inversi´on (incluyendo variaci´on de inventarios), ne_gdi_totl_kn. -- Inversi´on (incluyendo variaci´on de inventarios) =


df4= df3 D.|> D.derive "log_ny_gdp_mktp_kn" (log ny_gdp_mktp_kn)
         D.|> D.derive "log_ne_con_totl_kn" (log ne_con_totl_kn)
         D.|> D.derive "log_ne_gdi_totl_kn" (log ne_gdi_totl_kn)



Px.show $ Px.line df4 [Px.x "year", Px.y "log_ny_gdp_mktp_kn" ]
Px.show $ Px.line df4 [Px.x "year", Px.y "log_ne_con_totl_kn" ]
Px.show $ Px.line df4 [Px.x "year", Px.y "log_ne_gdi_totl_kn" ]

-- log_ny_gdp_mktp_kn

-- m  = M.toMatrixCols ["y", "c", "i"] df4   -- Matrix Double, 66×3 (rows = years)
-- yv = M.columnVector "y" df4               -- Vector Double (one column)
-- mAll = M.toMatrix df4                     -- every column (all must be numeric)

-- pp df4

pbi=M.columnVector "log_ny_gdp_mktp_kn" df4
consumption=M.columnVector "log_ne_con_totl_kn" df4
investment=M.columnVector "log_ne_gdi_totl_kn" df4

n=size pbi
idn=ident n 

-- -1*subVector 0 (n-1) (idn ! 1)
-- -1*subVector 0 (n-1) (idn ! 2)
-- subVector 1 (n-1) (idn ! 1)
-- subVector 1 (n-1) (idn ! 2)

minus= -1*subMatrix (1,0) (n-1,n) idn
zeroes_row=asRow (fromList [0 | x<-[0..n-1]])

diff=idn+(minus === zeroes_row)
diff2_pre=diff LA.<> diff
diff2= subMatrix (0,0) (n-2,n) diff2_pre

lambda=100

weird = tr diff2 LA.<> diff2
toinv= idn + (lambda * weird)
tomult = inv toinv

pbi_trend_100=tomult LA.<> reshape 1 pbi
consumption_trend_100=tomult LA.<> reshape 1 consumption
investment_trend_100=tomult LA.<> reshape 1 investment


df5= df4 D.|> D.insert "pbi_trend" (toList (flatten pbi_trend_100)) 
         D.|> D.insert "consumption_trend" (toList (flatten consumption_trend_100)) 
         D.|> D.insert "investment_trend" (toList (flatten investment_trend_100)) 

Px.show $ Px.line df5 [Px.x "year", Px.ys ["log_ny_gdp_mktp_kn", "pbi_trend"] ]
Px.show $ Px.line df5 [Px.x "year", Px.ys ["log_ne_con_totl_kn", "consumption_trend"] ]
Px.show $ Px.line df5 [Px.x "year", Px.ys ["log_ne_gdi_totl_kn", "investment_trend"] ]

pbi_cycles= -1*(pbi_trend_100 - reshape 1 pbi)
consumption_cycles= -1*(consumption_trend_100 - reshape 1 consumption)
investment_cycles= -1*(investment_trend_100 - reshape 1 investment)

pbi_dot=(tr pbi_cycles LA.<> pbi_cycles)
consumption_dot=(tr consumption_cycles LA.<> consumption_cycles)
investment_dot=(tr investment_cycles LA.<> investment_cycles)

pbi_std = sqrt $ (pbi_dot ! 0 ! 0)/fromIntegral (n -1)
consumption_std = sqrt $ (consumption_dot ! 0 ! 0)/fromIntegral (n -1)
investment_std = sqrt $ (investment_dot ! 0 ! 0)/fromIntegral (n -1)

pbi_std
consumption_std
investment_std

consumption_std/pbi_std
investment_std/pbi_std

df6= df5 D.|> D.insert "pbi_cycles" (toList (flatten pbi_cycles)) 
         D.|> D.insert "consumption_cycles" (toList (flatten consumption_cycles)) 
         D.|> D.insert "investment_cycles" (toList (flatten investment_cycles)) 

Px.show $ Px.histogram df6 [Px.x "pbi_cycles", Px.nbins 20]
Px.show $ Px.histogram df6 [Px.x "consumption_cycles", Px.nbins 20]
Px.show $ Px.histogram df6 [Px.x "investment_cycles", Px.nbins 20]

cov_pbi_cons=(tr pbi_cycles LA.<> consumption_cycles) ! 0 ! 0 / fromIntegral (n -1)
cov_pbi_inv=(tr pbi_cycles LA.<> investment_cycles) ! 0 ! 0 / fromIntegral (n - 1)
corr_pbi_cons=cov_pbi_cons/(pbi_std*consumption_std)
corr_pbi_inv=cov_pbi_inv/(pbi_std*investment_std)

corr_pbi_cons
corr_pbi_inv
