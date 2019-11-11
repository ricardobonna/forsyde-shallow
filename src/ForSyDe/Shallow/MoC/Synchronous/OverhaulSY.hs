-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.Synchronous.OverhaulSY
-- Copyright   :  (c) ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronous process library overhauled for classified purposes
-----------------------------------------------------------------------------
module ForSyDe.Shallow.MoC.Synchronous.OverhaulSY (
  constant, delta, sat, varSat, safeDiv, folpf, integrate
  ) where

import ForSyDe.Shallow.MoC.Synchronous.Lib
import ForSyDe.Shallow.Core

instance (Num a) => Num (Signal a) where
  (+) = zipWithSY (+)
  (-) = zipWithSY (-)
  (*) = zipWithSY (*)
  abs = mapSY abs
  signum = mapSY signum
  fromInteger = signal . repeat . fromInteger

instance (Fractional a) => Fractional (Signal a) where
  (/) = zipWithSY (/)
  fromRational = signal . repeat . fromRational

instance (Floating a) => Floating (Signal a) where
  pi = signal $ repeat pi
  exp = mapSY exp
  log = mapSY log
  sin = mapSY sin
  cos = mapSY cos
  asin = mapSY asin
  acos = mapSY acos
  atan = mapSY atan
  sinh = mapSY sinh
  cosh = mapSY cosh
  asinh = mapSY asinh
  acosh = mapSY acosh
  atanh = mapSY atanh

instance (Ord a) => Ord (Signal a) where
  max = zipWithSY max
  min = zipWithSY min

instance (Real a) => Real (Signal a) where
  toRational = toRational . headS

constant :: a -> Signal a
constant = signal . repeat

delta :: (Num a) => Signal Bool -> Signal a -> Signal a
delta reset sig = zipWith3SY deltaFunc reset sig (delaySY 0 sig)
  where deltaFunc True _ _ = 0
        deltaFunc False s ds = s - ds

sat :: (Ord a) => (a, a) -> Signal a -> Signal a
sat (minVal, maxVal) = mapSY (max minVal . min maxVal)

varSat :: (Ord a) => Signal a -> Signal a -> Signal a -> Signal a
varSat sl sh = max sl . min sh

safeDiv :: (Fractional a, Ord a) => a -> Signal a -> Signal a -> Signal a
safeDiv e = zipWithSY (safeDivFunc e)
  where safeDivFunc eps num den
          | den > 0 = num / safeDen
          | otherwise = num / negate safeDen
          where safeDen = max (abs eps) (abs den)

folpf :: (Fractional a) => (a, a) -> Signal Bool -> Signal a -> Signal a
folpf (period, tau) reset inp = out
  where out = zipWith4SY folpfFunc reset inp dinp dout
        dinp = delaySY Abst (mapSY Prst inp)
        dout = delaySY 0 out
        a = period / (2*tau+period)
        b = (2*tau-period) / (2*tau+period)
        folpfFunc True x _ _ = x
        folpfFunc _ x Abst _ = x
        folpfFunc False x (Prst dx) dy = b*dy + a*(x + dx)

integrate :: (Fractional a) => (a, a) -> Signal Bool -> Signal a -> Signal a
integrate (period, x0) reset inp = out
  where out = zipWith4SY integrateFunc reset inp dinp dout
        dinp = delaySY 0 inp
        dout = delaySY x0 out
        integrateFunc True _ _ _ = x0
        integrateFunc False x dx dy = dy + period/2*(x + dx)
