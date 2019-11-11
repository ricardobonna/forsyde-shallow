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
-- The synchronous process library overhauled for unknown purposes
-----------------------------------------------------------------------------
module ForSyDe.Shallow.MoC.Synchronous.OverhaulSY (
  constant, delta, sat, varSat, safeDiv, folpf, integrate, switch, greater,
  smaller, equal, grequal, ltequal, (|||), (&&&)
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


---------------------------------------------
------------- Usefull library ---------------
---------------------------------------------

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

switch :: Signal Bool -> Signal a -> Signal a -> Signal a
switch = zipWith3SY (\s x1 x2 -> if s then x1 else x2)

safeDiv :: (Fractional a, Ord a) => a -> Signal a -> Signal a -> Signal a
safeDiv e = zipWithSY (safeDivFunc e)
  where safeDivFunc eps num den
          | den > 0 = num / safeDen
          | otherwise = num / negate safeDen
          where safeDen = max (abs eps) (abs den)

folpf :: (Fractional a) => (a, a) -> Signal Bool -> Signal a -> Signal a
folpf (period, tau) reset inp = out
  where out = headS inp :- switch (tailS reset) (tailS inp) (b*out + a*(tailS inp + inp))
        a = constant $ period / (2*tau+period)
        b = constant $ (2*tau-period) / (2*tau+period)

integrate :: (Fractional a) => a -> Signal Bool -> Signal a -> Signal a -> Signal a
integrate period reset inp x0 = out
  where out = headS x0 :- switch (tailS reset) (tailS x0) (out + a*(tailS inp + inp))
        a = constant $ period / 2

greater :: (Ord a) => Signal a -> Signal a -> Signal Bool
greater = zipWithSY (>)

equal :: (Eq a) => Signal a -> Signal a -> Signal Bool
equal = zipWithSY (==)

smaller :: (Ord a) => Signal a -> Signal a -> Signal Bool
smaller = zipWithSY (<)

grequal :: (Ord a) => Signal a -> Signal a -> Signal Bool
grequal = zipWithSY (>=)

ltequal :: (Ord a) => Signal a -> Signal a -> Signal Bool
ltequal = zipWithSY (<=)

(|||) :: Signal Bool -> Signal Bool -> Signal Bool
(|||) = zipWithSY (||)
infixr 2 |||

(&&&) :: Signal Bool -> Signal Bool -> Signal Bool
(&&&) = zipWithSY (&&)
infixr 3 &&&
