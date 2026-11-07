-------------------------------------------------------------------------------------------------------------------------------------------------HS

:set -XOverloadedStrings
:add PlotlyExpress.hs DfMatrix.hs Pretty.hs MatQuad.hs Reshape.hs HpFilter.hs
import HpFilter
:module -*Reshape
import qualified PlotlyExpress as Px
import qualified DfMatrix as M
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import Pretty
import qualified Reshape as R
import MatQuad
import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as LA
import qualified Data.List as L
--------------------------------------------------------------------HF
Ok, 8 modules loaded.
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- solve generic stable cuadratic
let qTrue = (3><3) [ 0.9, 0.1, 0.0
                   , 0.0, 0.5, 0.2
                   , 0.1, 0.0, 0.3 ] :: Matrix Double

let a = (3><3) [ 1.0, 0.2, 0.0
               , 0.1, 1.0, 0.3
               , 0.0, 0.2, 1.0 ] :: Matrix Double

let b = (3><3) [ -3.0, 0.5,  0.1
               ,  0.2, -2.5, 0.4
               ,  0.1, 0.3, -2.8 ] :: Matrix Double


-- negate ((a <> qTrue <> qTrue) + (b <> qTrue))

let c = (3><3) [   1.876, -0.14, -0.182
                   , -0.357, 0.963,  0.191
                   , 0.066,  -0.22,  0.658 ] :: Matrix Double

a<>qTrue<>qTrue+b<>qTrue+c



--------------------------------------------------------------------HF
(3><3)
 [ -4.440892098500626e-16, -5.551115123125783e-17, 0.0
 ,  5.551115123125783e-17,                    0.0, 0.0
 ,  6.938893903907228e-17,                    0.0, 0.0 ]
-------------------------------------------------------------------------------------------------------------------------------------------------HS

solve_cuadratic a b c = z11 <> inv z21
  where
        s=fst $ size a
        ceroes=konst 0 (s,s) :: Matrix Double
        cc=fromBlocks [[-b, -c ], [ident s, ceroes]]
        aa=fromBlocks [[a,ceroes],[ceroes,ident s]]
        Right r = qz (1 + 1e-6) cc aa
        sd=qzSdim r
        z11=qzZ r ?? (Take sd, Take sd)
        z21=qzZ r ?? (TakeLast sd, Take sd)

solve_cuadratic a b c 
qTrue
--------------------------------------------------------------------HF
(3><3)
 [     0.8999999999999982,  9.999999999999973e-2, 2.914335439641036e-16
 , -9.367506770274758e-16,    0.4999999999999995,   0.20000000000000026
 ,    0.10000000000000045, 5.377642775528102e-16,   0.30000000000000043 ]
(3><3)
 [ 0.9, 0.1, 0.0
 , 0.0, 0.5, 0.2
 , 0.1, 0.0, 0.3 ]
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- loading blocks

-- calibration
beta=0.99
theta=0.36
delta=0.025
awork=2
h0=0.53
gamma=0.95
sigma=0.00712

-- steady states
r_ss=delta-1+1/beta
lambda_ss=1
y_ss=(1-theta)/(1-theta+awork*(1-delta*theta/r_ss))*((theta/r_ss)**(theta/(1-theta)))
c_ss=(1-delta*theta/r_ss)*y_ss
h_ss=(1-theta)/((1-theta)+awork*c_ss/y_ss)
k_ss=(theta/r_ss)*y_ss
i_ss=delta*k_ss


-- uhlig matrices
-- block 1
-- households
c_hh = (3><3) [-1, -h_ss/(1-h_ss), 0
              , 0,0,delta
              , -c_ss/y_ss,1-theta,-i_ss/y_ss] :: Matrix Double
c_hp = (3><2) [1, 0 
              , 0,0
              , 1-theta,theta] :: Matrix Double
-- firms 
c_ff = (3><3) [1, 0, -1
              , 1,-1,0
              , -1,1-theta,theta] :: Matrix Double
c_fp = (3><2) [0, -1
              , -1,0
              , 0,0] :: Matrix Double

-- market clearing
c_mh = (2><3) [ 0,1,0
              , 0,0,0] :: Matrix Double

c_mf = (2><3) [ 0,-1,0
              , 0,0,-1] :: Matrix Double

c=fromBlocks [[c_hh,0,c_hp],[0, c_ff, c_fp],[c_mh,c_mf,0]]

a= (8><1) [0,-1,0,0,0,0,0,0] :: Matrix Double
b= (8><1) [0,1-delta,theta,0,0,0,0,1] :: Matrix Double
d= (8><1) [0,0,0,0,0,1,0,0] :: Matrix Double

-- expected values block 
k= (1><8) [1,0,0,0,0,0,0,0] :: Matrix Double
j= (1><8) [-1,0,0,0,0,0,0,1-beta*(1-delta)] :: Matrix Double

-- exogenous blocks
n=(1><1) [gamma] :: Matrix Double
number_exogenous=fst $ size n

a

--------------------------------------------------------------------HF
(8><1)
 [  0.0
 , -1.0
 ,  0.0
 ,  0.0
 ,  0.0
 ,  0.0
 ,  0.0
 ,  0.0 ]
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- uhlig algorithm

uhlig a b c d j k = (q,p,r,o)
        where
                q0=k<>inv c<>b
                q1=k<>inv c<>a+j<>inv c<>b
                q2=j<>inv c<>a
                q=solve_cuadratic q2 q1 q0 
                p= -inv c<>(b+a<>q)
                r=(k<>inv c<>d+j<>inv c<>d<>n)<>inv (( -k<>inv c<>a+j<>p)<>ident number_exogenous - j<>inv c<>a<>n)
                o= -inv c<>(a<>r+d)

(q,p,r,o)=uhlig a b c d j k 
(q,p,r,o)
--------------------------------------------------------------------HF
((1><1)
 [ 0.9528023151153538 ],(8><1)
 [  0.5659818944154773
 , -0.2606242239744563
 , -0.8879073953858444
 , 0.19320049665634792
 , -0.2606242239744563
 ,                 1.0
 , 0.45382472063080426
 , -0.8067995033436521 ],(1><1)
 [ 0.11616968246565998 ],(8><1)
 [ 0.3980545679280074
 , 0.7616279166376281
 ,    4.6467872986264
 , 1.4874418666480818
 , 0.7616279166376281
 ,               -0.0
 , 0.7258139500104539
 , 1.4874418666480818 ])
-------------------------------------------------------------------------------------------------------------------------------------------------HS

-- unfiltered variances
tt= fromBlocks [[q, r<>n], [0,n]]
vv= r === 1
sigE = (1><1) [sigma**2]

vlamb = sigma**2/(1-gamma**2)
nS = LA.rows tt

sigS = reshape nS $ inv (ident (nS*nS) - kronecker tt tt) #> flatten (vv <> sigE <> tr vv)

-- jump variables
ww   = fromBlocks [[p, o<>n]]
varY = ww <> sigS <> tr ww + o <> sigE <> tr o

-- unfiltered variances
cmap (\v -> 100*sqrt v) (takeDiag varY)    -- std. dev. in %
--------------------------------------------------------------------HF
[2.906750486329254,1.2856377953168094,8.625480531866039,3.9436040370021423,1.2856377953168094,3.910715658943851
6,3.1424349934948728,2.633764572648093]
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- filtered variances
-- first take from the steady distribution, and then do the simulation
allLambdaErrors <- (* realToFrac sigma) <$> randn 100 115
starting_points = gaussianSample 42 100 (konst 0 2) (trustSym sigS)

starting_point =(-6.11424425125707e-3, -3.4556648534822937e-3)
errs_0=toList $ allLambdaErrors ! 0

k_0=fst starting_point
lambda_0=snd starting_point


simulation (q,p,r,o) _ _ [] = []
simulation (q,p,r,o) k_t lambda_t  (err_t1:next_errs) = ((k_t,lambda_t, y_t):simulation (q,p,r,o) k_t1 lambda_t1 next_errs) 
        where 
                k_t1=q<>k_t+r*lambda_t
                lambda_t1=gamma*lambda_t+scalar err_t1
                y_t=p<>k_t+o<>lambda_t

simulated=simulation (q,p,r,o) k_0 lambda_0 errs_0 



names = ["c","h","i","y","h","k","w","r"]
series = toRows $ fromColumns [flatten y | (_,_,y) <- simulated]
dfSim = foldl (\df (nm, v) -> df D.|> D.insert nm (toList v))
              (D.empty D.|> D.insert "t" ([0..114] :: [Int]))
              (zip names series)

Px.show $ Px.line dfSim [Px.x "t", Px.ys ["y","c","i"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["k"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["h"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["w"]]

--------------------------------------------------------------------HF
[html] charts/by-id/1794049977677571.html 1794049977678
[html] charts/by-id/1794049977714121.html 1794049977714
[html] charts/by-id/1794049977730319.html 1794049977730
[html] charts/by-id/1794049977744002.html 1794049977744
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- y, c, i, k, h, w
ys = (115><1) (toList (M.columnVector "y" dfSim)) :: Matrix Double
cs = (115><1) (toList (M.columnVector "c" dfSim)) :: Matrix Double
is = (115><1) (toList (M.columnVector "i" dfSim)) :: Matrix Double
ks = (115><1) (toList (M.columnVector "k" dfSim)) :: Matrix Double
hs = (115><1) (toList (M.columnVector "h" dfSim)) :: Matrix Double
ws = (115><1) (toList (M.columnVector "w" dfSim)) :: Matrix Double

ytrend= hpTrend 1600 ys
ctrend= hpTrend 1600 cs
itrend= hpTrend 1600 is
ktrend= hpTrend 1600 ks
htrend= hpTrend 1600 hs
wtrend= hpTrend 1600 ws

ycycle=ys-ytrend
ccycle=cs-ctrend
icycle=is-itrend
kcycle=ks-ktrend
hcycle=hs-htrend
wcycle=ws-wtrend

ystd=stdDev (ys-ytrend)
cstd=stdDev (cs-ctrend)
istd=stdDev (is-itrend)
kstd=stdDev (ks-ktrend)
hstd=stdDev (hs-htrend)
wstd=stdDev (ws-wtrend)

ystd
cstd
istd
kstd
hstd
wstd


--------------------------------------------------------------------HF
1.1893401272804978e-2
3.747414375007563e-3
3.704457791484385e-2
3.3341748370769152e-3
6.101029320396208e-3
6.029760625402377e-3
-------------------------------------------------------------------------------------------------------------------------------------------------HS

dfCycles = dfSim D.|> D.insert "y_cycle" (toList (flatten ycycle))
                 D.|> D.insert "c_cycle" (toList (flatten ccycle))
                 D.|> D.insert "i_cycle" (toList (flatten icycle))
                 D.|> D.insert "k_cycle" (toList (flatten kcycle))
                 D.|> D.insert "h_cycle" (toList (flatten hcycle))
                 D.|> D.insert "w_cycle" (toList (flatten wcycle))


Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["y_cycle","c_cycle","i_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]

Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["k_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]

Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["h_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]

Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["w_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]
--------------------------------------------------------------------HF
[html] charts/by-id/1794049979630249.html 1794049979630
[html] charts/by-id/1794049979685181.html 1794049979685
[html] charts/by-id/1794049979710213.html 1794049979710
[html] charts/by-id/1794049979741433.html 1794049979741
-------------------------------------------------------------------------------------------------------------------------------------------------HS
--inpulse function
(k_0,lambda_0) =(0, 1)
errs_0= [0 |x<-[0..114]]


simulated=simulation (q,p,r,o) k_0 lambda_0 errs_0 



names = ["c","h","i","y","h","k","w","r"]
series = toRows $ fromColumns [flatten y | (_,_,y) <- simulated]
dfSim = foldl (\df (nm, v) -> df D.|> D.insert nm (toList v))
              (D.empty D.|> D.insert "t" ([0..114] :: [Int]))
              (zip names series)

Px.show $ Px.line dfSim [Px.x "t", Px.ys ["y","c","i"]]

Px.show $ Px.line dfSim [Px.x "t", Px.ys ["k"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["h"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["w"]]
--------------------------------------------------------------------HF
[html] charts/by-id/1794049981039280.html 1794049981039
[html] charts/by-id/1794049981066359.html 1794049981066
[html] charts/by-id/1794049981078328.html 1794049981078
[html] charts/by-id/1794049981095651.html 1794049981096
-------------------------------------------------------------------------------------------------------------------------------------------------HS

oneRun i = (stds, corrs)
  where
    sim    = simulation (q,p,r,o)
                        (scalar (starting_points ! i ! 0))
                        (scalar (starting_points ! i ! 1))
                        (toList (allLambdaErrors ! i))
    ysim   = fromColumns [flatten yy | (_,_,yy) <- sim]     -- 8 × 115, row = variable
    var j  = hpCycle 1600 (asColumn (ysim ! j))             -- HP cycle of variable j
    [cC, hC, iC, yC, kC, wC] = map var [0, 1, 2, 3, 5, 6]   -- c h i y k w
    vars   = [yC, cC, iC, kC, hC, wC]
    stds   = map (\v -> 100 * stdDev v) vars
    corrs  = map (\v -> correlation v yC) vars

runs = map oneRun [0..99]
allStds  = L.transpose (map fst runs)      -- 6 lists, each with 100 values
allCorrs = L.transpose (map snd runs)

col = asColumn . fromList
names = ["Output","Consumption","Investment","Capital","Hours","Productivity"]

table = zip3 names
          [ (mean (col s), stdDev (col s)) | s <- allStds  ]
          [ (mean (col c), stdDev (col c)) | c <- allCorrs ]

mapM_ print table
--------------------------------------------------------------------HF
("Output",(1.3481345805993175,0.18404968348849177),(1.0,1.262401835544405e-16))
("Consumption",(0.41959650415489025,7.086986224993674e-2),(0.8925621835029527,1.9015575745546534e-2))
("Investment",(4.210488197273001,0.5614657301637032),(0.9913575620372583,2.3651107138220077e-3))
("Capital",(0.3707025417184638,8.525666824225912e-2),(5.7484325819148294e-2,7.24027722968347e-2))
("Hours",(0.6942075541713337,9.209129906078149e-2),(0.9814139676301744,5.1640726436166275e-3))
("Productivity",(0.6802143565986337,9.936081880810725e-2),(0.9808056157054736,4.419574551459346e-3))
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- indivisible labor
bwork = -awork*log(1-h0)/h0

-- steady states
r_ss=delta-1+1/beta
lambda_ss=1
c_y=1-delta*theta/r_ss
h_ss=(1-theta)/(bwork*c_y)
y_ss=h_ss*((theta/r_ss)**(theta/(1-theta)))
c_ss=c_y*y_ss
k_ss=(theta/r_ss)*y_ss
i_ss=delta*k_ss

bwork

--------------------------------------------------------------------HF
2.849141827464275
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- uhlig matrices
-- block 1
-- households
c_hh = (3><3) [-1, 0, 0
              , 0,0,delta
              , -c_ss/y_ss,1-theta,-i_ss/y_ss] :: Matrix Double
c_hp = (3><2) [1, 0
              , 0,0
              , 1-theta,theta] :: Matrix Double
-- firms
c_ff = (3><3) [1, 0, -1
              , 1,-1,0
              , -1,1-theta,theta] :: Matrix Double
c_fp = (3><2) [0, -1
              , -1,0
              , 0,0] :: Matrix Double

-- market clearing
c_mh = (2><3) [ 0,1,0
              , 0,0,0] :: Matrix Double

c_mf = (2><3) [ 0,-1,0
              , 0,0,-1] :: Matrix Double

c=fromBlocks [[c_hh,0,c_hp],[0, c_ff, c_fp],[c_mh,c_mf,0]]

a= (8><1) [0,-1,0,0,0,0,0,0] :: Matrix Double
b= (8><1) [0,1-delta,theta,0,0,0,0,1] :: Matrix Double
d= (8><1) [0,0,0,0,0,1,0,0] :: Matrix Double

-- expected values block
k= (1><8) [1,0,0,0,0,0,0,0] :: Matrix Double
j= (1><8) [-1,0,0,0,0,0,0,1-beta*(1-delta)] :: Matrix Double

-- exogenous blocks
n=(1><1) [gamma] :: Matrix Double
number_exogenous=fst $ size n

--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- uhlig
(q,p,r,o)=uhlig a b c d j k 
(q,p,r,o)
--------------------------------------------------------------------HF
((1><1)
 [ 0.9418166596902471 ],(8><1)
 [   0.5315878086354405
 ,  -0.4766328017651127
 ,  -1.3273336123901156
 , 5.495500687032795e-2
 ,  -0.4766328017651127
 ,                  1.0
 ,   0.5315878086354405
 ,   -0.945044993129672 ],(1><1)
 [ 0.15522831444011556 ],(8><1)
 [ 0.47027449858208203
 ,  1.4714597261608837
 ,   6.209132577604622
 ,  1.9417342247429654
 ,  1.4714597261608837
 ,                -0.0
 , 0.47027449858208203
 ,  1.9417342247429654 ])
-------------------------------------------------------------------------------------------------------------------------------------------------HS

-- unfiltered variances
tt= fromBlocks [[q, r<>n], [0,n]]
vv= r === 1
sigE = (1><1) [sigma**2]

vlamb = sigma**2/(1-gamma**2)
nS = LA.rows tt

sigS = reshape nS $ inv (ident (nS*nS) - kronecker tt tt) #> flatten (vv <> sigE <> tr vv)

-- jump variables
ww   = fromBlocks [[p, o<>n]]
varY = ww <> sigS <> tr ww + o <> sigE <> tr o

-- unfiltered variances
cmap (\v -> 100*sqrt v) (takeDiag varY)    -- std. dev. in %

--------------------------------------------------------------------HF
[3.229744019276986,2.3612609915650165,10.750133289021178,4.606322430727423,2.3612609915650165,4.467426936734372
,3.229744019276986,3.2707917070408916]
-------------------------------------------------------------------------------------------------------------------------------------------------HS

-- filtered variances
-- first take from the steady distribution, and then do the simulation
-- allLambdaErrors <- (* realToFrac sigma) <$> randn 100 115
-- starting_points = gaussianSample 42 100 (konst 0 2) (trustSym sigS)

starting_point =(-6.11424425125707e-3, -3.4556648534822937e-3)
errs_0=toList $ allLambdaErrors ! 0

k_0=fst starting_point
lambda_0=snd starting_point


simulation (q,p,r,o) _ _ [] = []
simulation (q,p,r,o) k_t lambda_t  (err_t1:next_errs) = ((k_t,lambda_t, y_t):simulation (q,p,r,o) k_t1 lambda_t1 next_errs) 
        where 
                k_t1=q<>k_t+r*lambda_t
                lambda_t1=gamma*lambda_t+scalar err_t1
                y_t=p<>k_t+o<>lambda_t

simulated=simulation (q,p,r,o) k_0 lambda_0 errs_0 



names = ["c","h","i","y","h","k","w","r"]
series = toRows $ fromColumns [flatten y | (_,_,y) <- simulated]
dfSim = foldl (\df (nm, v) -> df D.|> D.insert nm (toList v))
              (D.empty D.|> D.insert "t" ([0..114] :: [Int]))
              (zip names series)

Px.show $ Px.line dfSim [Px.x "t", Px.ys ["y","c","i"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["k"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["h"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["w"]]
--------------------------------------------------------------------HF
[html] charts/by-id/1794049987650416.html 1794049987650
[html] charts/by-id/1794049987682967.html 1794049987683
[html] charts/by-id/1794049987697773.html 1794049987698
[html] charts/by-id/1794049987713861.html 1794049987714
-------------------------------------------------------------------------------------------------------------------------------------------------HS

-- y, c, i, k, h, w
ys = (115><1) (toList (M.columnVector "y" dfSim)) :: Matrix Double
cs = (115><1) (toList (M.columnVector "c" dfSim)) :: Matrix Double
is = (115><1) (toList (M.columnVector "i" dfSim)) :: Matrix Double
ks = (115><1) (toList (M.columnVector "k" dfSim)) :: Matrix Double
hs = (115><1) (toList (M.columnVector "h" dfSim)) :: Matrix Double
ws = (115><1) (toList (M.columnVector "w" dfSim)) :: Matrix Double

ytrend= hpTrend 1600 ys
ctrend= hpTrend 1600 cs
itrend= hpTrend 1600 is
ktrend= hpTrend 1600 ks
htrend= hpTrend 1600 hs
wtrend= hpTrend 1600 ws

ycycle=ys-ytrend
ccycle=cs-ctrend
icycle=is-itrend
kcycle=ks-ktrend
hcycle=hs-htrend
wcycle=ws-wtrend

ystd=stdDev (ys-ytrend)
cstd=stdDev (cs-ctrend)
istd=stdDev (is-itrend)
kstd=stdDev (ks-ktrend)
hstd=stdDev (hs-htrend)
wstd=stdDev (ws-wtrend)

ystd
cstd
istd
kstd
hstd
wstd
--------------------------------------------------------------------HF
1.5491738193083901e-2
4.539791017559436e-3
4.9426401692591854e-2
4.365317515493275e-3
1.1769336926769207e-2
4.539791017559436e-3
-------------------------------------------------------------------------------------------------------------------------------------------------HS

dfCycles = dfSim D.|> D.insert "y_cycle" (toList (flatten ycycle))
                 D.|> D.insert "c_cycle" (toList (flatten ccycle))
                 D.|> D.insert "i_cycle" (toList (flatten icycle))
                 D.|> D.insert "k_cycle" (toList (flatten kcycle))
                 D.|> D.insert "h_cycle" (toList (flatten hcycle))
                 D.|> D.insert "w_cycle" (toList (flatten wcycle))

Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["y_cycle","c_cycle","i_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]

Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["k_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]

Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["h_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]

Px.show $ Px.line dfCycles [Px.x "t", Px.ys ["w_cycle"], Px.title "Ciclos (HP, lambda, 1600)"]
--------------------------------------------------------------------HF
[html] charts/by-id/1794049989756419.html 1794049989756
[html] charts/by-id/1794049989792277.html 1794049989792
[html] charts/by-id/1794049989811984.html 1794049989812
[html] charts/by-id/1794049989829201.html 1794049989829
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--inpulse function
(k_0,lambda_0) =(0, 1)
errs_0= [0 |x<-[0..114]]


simulated=simulation (q,p,r,o) k_0 lambda_0 errs_0 


starting_point

names = ["c","h","i","y","h","k","w","r"]
series = toRows $ fromColumns [flatten y | (_,_,y) <- simulated]
dfSim = foldl (\df (nm, v) -> df D.|> D.insert nm (toList v))
              (D.empty D.|> D.insert "t" ([0..114] :: [Int]))
              (zip names series)

Px.show $ Px.line dfSim [Px.x "t", Px.ys ["y","c","i"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["k"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["h"]]
Px.show $ Px.line dfSim [Px.x "t", Px.ys ["w"]]

--------------------------------------------------------------------HF
(-6.11424425125707e-3,-3.4556648534822937e-3)
[html] charts/by-id/1794049990714099.html 1794049990714
[html] charts/by-id/1794049990769859.html 1794049990770
[html] charts/by-id/1794049990797024.html 1794049990797
[html] charts/by-id/1794049990820926.html 1794049990821
-------------------------------------------------------------------------------------------------------------------------------------------------HS


oneRun i = (stds, corrs)
  where
    sim    = simulation (q,p,r,o)
                        (scalar (starting_points ! i ! 0))
                        (scalar (starting_points ! i ! 1))
                        (toList (allLambdaErrors ! i))
    ysim   = fromColumns [flatten yy | (_,_,yy) <- sim]     -- 8 × 115, row = variable
    var j  = hpCycle 1600 (asColumn (ysim ! j))             -- HP cycle of variable j
    [cC, hC, iC, yC, kC, wC] = map var [0, 1, 2, 3, 5, 6]   -- c h i y k w
    vars   = [yC, cC, iC, kC, hC, wC]
    stds   = map (\v -> 100 * stdDev v) vars
    corrs  = map (\v -> correlation v yC) vars

runs = map oneRun [0..99]
allStds  = L.transpose (map fst runs)      -- 6 lists, each with 100 values
allCorrs = L.transpose (map snd runs)

col = asColumn . fromList
names = ["Output","Consumption","Investment","Capital","Hours","Productivity"]

table = zip3 names
          [ (mean (col s), stdDev (col s)) | s <- allStds  ]
          [ (mean (col c), stdDev (col c)) | c <- allCorrs ]

mapM_ print table
--------------------------------------------------------------------HF
("Output",(1.757465377421603,0.23805046631142335),(1.0,1.3846913936850747e-16))
("Consumption",(0.5090380227551279,8.822514383619992e-2),(0.8694502854522885,2.20395752424131e-2))
("Investment",(5.622357396619273,0.7434915949549942),(0.9914353893645798,2.3544103818743736e-3))
("Capital",(0.4887968755663302,0.1127138335011706),(5.192207727996129e-2,7.170729855596233e-2))
("Hours",(1.3403041116456273,0.17632348702319106),(0.9819805288984651,5.023986995287401e-3))
("Productivity",(0.5090380227551279,8.822514383619992e-2),(0.8694502854522885,2.20395752424131e-2))
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
