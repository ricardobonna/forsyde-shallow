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
  constant, delta, sat, varSat
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

delta :: (Num a) => Signal a -> Signal Bool -> Signal a
delta sig = zipWith3SY deltaFunc sig (delaySY 0 sig)
  where deltaFunc _ _ True = 0
        deltaFunc s ds False = s - ds

sat :: (Ord a) => a -> a -> Signal a -> Signal a
sat x y = mapSY (max x . min y)

varSat :: (Ord a) => Signal a -> Signal a -> Signal a -> Signal a
varSat sl sh = max sl . min sh
