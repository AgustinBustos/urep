{-# LANGUAGE ForeignFunctionInterface #-}
-- | QZ (generalized Schur, LAPACK dgges) and the matrix quadratic
--   A0 + A1 X + A2 X^2 = 0  (stable solvent), for hmatrix matrices.
--   LAPACK is already linked by hmatrix, so no extra packages are needed.
module MatQuad
  ( QZ(..), qz, solveQuadratic, residual
  ) where

import Prelude hiding ((<>))
import Foreign
import Foreign.C.Types
import Foreign.C.String (withCString)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Numeric.LinearAlgebra

-- Fortran LOGICAL selector: (alphar, alphai, beta) -> keep in leading block?
type Sel = Ptr Double -> Ptr Double -> Ptr Double -> IO CInt
foreign import ccall "wrapper" mkSel :: Sel -> IO (FunPtr Sel)

-- DGGES(JOBVSL,JOBVSR,SORT,SELCTG,N,A,LDA,B,LDB,SDIM,ALPHAR,ALPHAI,BETA,
--       VSL,LDVSL,VSR,LDVSR,WORK,LWORK,BWORK,INFO) + 3 hidden Fortran string lengths.
-- `safe` because LAPACK calls back into Haskell (the selector).
foreign import ccall safe "dgges_" c_dgges
  :: Ptr CChar -> Ptr CChar -> Ptr CChar -> FunPtr Sel
  -> Ptr CInt -> Ptr Double -> Ptr CInt -> Ptr Double -> Ptr CInt -> Ptr CInt
  -> Ptr Double -> Ptr Double -> Ptr Double
  -> Ptr Double -> Ptr CInt -> Ptr Double -> Ptr CInt
  -> Ptr Double -> Ptr CInt -> Ptr CInt -> Ptr CInt
  -> CSize -> CSize -> CSize -> IO ()

-- | Result of Q^T A Z = S, Q^T B Z = T (S quasi-upper-triangular, T upper-triangular).
--   Eigenvalues are alpha/beta; the first 'qzSdim' are the selected (stable) ones.
data QZ = QZ
  { qzS, qzT, qzQ, qzZ :: Matrix Double
  , qzSdim  :: Int
  , qzAlpha :: Vector (Complex Double)
  , qzBeta  :: Vector Double
  } deriving Show

colMajor :: Matrix Double -> V.Vector Double
colMajor = flatten . tr

fromColMajor :: Int -> V.Vector Double -> Matrix Double
fromColMajor n = tr . reshape n

-- | Real QZ of the pencil (A, B), ordering eigenvalues with |alpha/beta| < crit first.
--   Infinite eigenvalues (beta = 0, i.e. singular B) are never selected.
qz :: Double -> Matrix Double -> Matrix Double -> Either String QZ
qz crit a b = unsafePerformIO $ do
  let n = rows a
      sel pr pim pb = do
        ar <- peek pr; ai <- peek pim; be <- peek pb
        pure (if sqrt (ar*ar + ai*ai) < crit * abs be then 1 else 0)
  fp <- mkSel sel
  ma <- V.thaw (colMajor a); mb <- V.thaw (colMajor b)
  mar <- MV.new n; mai <- MV.new n; mbe <- MV.new n
  mq <- MV.new (n*n); mz <- MV.new (n*n)
  bwork <- MV.new n :: IO (MV.IOVector CInt)
  let call lwork work =
        withCString "V" $ \jv -> withCString "S" $ \js ->
        with (fromIntegral n) $ \pn -> with (fromIntegral lwork) $ \plw ->
        alloca $ \psd -> alloca $ \pinfo ->
        MV.unsafeWith ma $ \pa -> MV.unsafeWith mb $ \pb ->
        MV.unsafeWith mar $ \par -> MV.unsafeWith mai $ \pai -> MV.unsafeWith mbe $ \pbe ->
        MV.unsafeWith mq $ \pq -> MV.unsafeWith mz $ \pz ->
        MV.unsafeWith work $ \pw -> MV.unsafeWith bwork $ \pbw -> do
          c_dgges jv jv js fp pn pa pn pb pn psd par pai pbe pq pn pz pn pw plw pbw pinfo 1 1 1
          (,) <$> peek psd <*> peek pinfo
  -- workspace query (lwork = -1); it doesn't touch A, B
  q <- MV.new 1
  _ <- call (-1 :: Int) q
  lw <- max (8*n + 16) . round <$> MV.read q 0
  work <- MV.new lw
  (sdim, info) <- call lw work
  freeHaskellFunPtr fp
  if info /= 0
    then pure (Left ("dgges failed, info = " ++ show info))
    else do
      s  <- V.freeze ma;  t  <- V.freeze mb
      vq <- V.freeze mq;  vz <- V.freeze mz
      ar <- V.freeze mar; ai <- V.freeze mai; be <- V.freeze mbe
      pure $ Right QZ
        { qzS = fromColMajor n s, qzT = fromColMajor n t
        , qzQ = fromColMajor n vq, qzZ = fromColMajor n vz
        , qzSdim = fromIntegral sdim
        , qzAlpha = V.zipWith (:+) ar ai, qzBeta = be }

-- | Stable solvent of  A0 + A1 X + A2 X^2 = 0,
--   i.e. of  A2 x_{t+1} + A1 x_t + A0 x_{t-1} = 0  with  x_t = X x_{t-1}.
--   A2 may be singular. Fails if the number of stable roots /= n (Blanchard–Kahn).
solveQuadratic :: Matrix Double -> Matrix Double -> Matrix Double -> Either String (Matrix Double)
solveQuadratic a0 a1 a2 = do
  let n  = rows a0
      i  = ident n
      o  = konst 0 (n, n)
      -- z_t = [x_{t-1}; x_t]:   E z = lambda D z
      e  = fromBlocks [[o, i], [-a0, -a1]]
      d  = fromBlocks [[i, o], [o, a2]]
  r <- qz (1 + 1e-6) e d
  if qzSdim r /= n
    then Left ("Blanchard-Kahn: " ++ show (qzSdim r) ++ " stable roots, need " ++ show n)
    else do
      let z   = qzZ r
          z11 = z ?? (Take n, Take n)
          z21 = z ?? (Drop n, Take n)
      -- X = Z21 Z11^{-1}  <=>  Z11^T X^T = Z21^T
      case linearSolve (tr z11) (tr z21) of
        Nothing -> Left "Z11 singular: no unique stable solution"
        Just xt -> Right (tr xt)

-- | ||A0 + A1 X + A2 X^2||_1
residual :: Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double -> Double
residual a0 a1 a2 x = norm_1 (a0 + (a1 <> x) + (a2 <> x <> x))
