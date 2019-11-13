-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.Synchronous.OverhaulSY
-- Copyright   :  (c) Ricardo Bonna, 2019
--                ForSyDe Group, KTH 2007-2008
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The synchronous process library overhauled for unknown purposes
-----------------------------------------------------------------------------
module ForSyDe.Shallow.MoC.Synchronous.OverhaulSY (
  (\==), (\!=), (\>), (\<), (\>=), (\<=), (\||), (\&&), true, false,
  constant, delta, sat, varSat, safeDiv, folpf, integrate, switch,
  switch3, latch, rateLim,
  x, dx, y, z, e
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
------------- Bool operations ---------------
---------------------------------------------

(\==) :: (Eq a) => Signal a -> Signal a -> Signal Bool
(\==) = zipWithSY (==)
infix 4 \==

(\!=) :: (Eq a) => Signal a -> Signal a -> Signal Bool
(\!=) = zipWithSY (/=)
infix 4 \!=

(\>) :: (Ord a) => Signal a -> Signal a -> Signal Bool
(\>) = zipWithSY (>)
infix 4 \>

(\<) :: (Ord a) => Signal a -> Signal a -> Signal Bool
(\<) = zipWithSY (<)
infix 4 \<

(\>=) :: (Ord a) => Signal a -> Signal a -> Signal Bool
(\>=) = zipWithSY (>=)
infix 4 \>=

(\<=) :: (Ord a) => Signal a -> Signal a -> Signal Bool
(\<=) = zipWithSY (<=)
infix 4 \<=

(\||) :: Signal Bool -> Signal Bool -> Signal Bool
(\||) = zipWithSY (||)
infixr 2 \||

(\&&) :: Signal Bool -> Signal Bool -> Signal Bool
(\&&) = zipWithSY (&&)
infixr 3 \&&

true :: Signal Bool
true = constant True

false :: Signal Bool
false = constant False


---------------------------------------------
------------- Usefull library ---------------
---------------------------------------------

constant :: a -> Signal a
constant = signal . repeat

delta :: (Num a) => Signal Bool -- ^ Reset signal
                 -> Signal a    -- ^ Input signal
                 -> Signal a    -- ^ Output signal
delta reset inp = switch reset (constant 0) (inp - (0:-inp))

sat :: (Ord a) => (a, a) -> Signal a -> Signal a
sat (minVal, maxVal) = mapSY (max minVal . min maxVal)

varSat :: (Ord a) => Signal a -> Signal a -> Signal a -> Signal a
varSat sl sh = max sl . min sh

switch :: Signal Bool -> Signal a -> Signal a -> Signal a
switch = zipWith3SY (\s x1 x2 -> if s then x1 else x2)

switch3 :: Signal Bool -> Signal a -> Signal Bool -> Signal a -> Signal a -> Signal a
switch3 = zipWith5SY (\s1 x1 s2 x2 x3 -> if s1 then x1 else if s2 then x2 else x3)

safeDiv :: (Fractional a, Ord a) => a -> Signal a -> Signal a -> Signal a
safeDiv e = zipWithSY (safeDivFunc e)
  where safeDivFunc eps num den
          | den > 0 = num / safeDen
          | otherwise = num / negate safeDen
          where safeDen = max (abs eps) (abs den)

folpf :: (Fractional a) => (a, a) -> Signal Bool -> Signal a -> Signal a
folpf _ NullS _ = NullS
folpf _ _ NullS = NullS
folpf (period, tau) (_:-reset) inp = out
  where out = headS inp :- switch reset (tailS inp) (b*out + a*(tailS inp + inp))
        a = constant $ period / (2*tau+period)
        b = constant $ (2*tau-period) / (2*tau+period)

integrate :: (Fractional a) => a -> Signal Bool -> Signal a -> Signal a -> Signal a
integrate _ NullS _ _ = NullS
integrate _ _ NullS _ = NullS
integrate _ _ _ NullS = NullS
integrate period (_:-reset) inp (x0:-x0s) = out
  where out = x0 :- switch reset x0s (out + a*(tailS inp + inp))
        a = constant $ period / 2

latch :: Signal Bool -> Signal Bool -> Signal Bool
latch NullS _ = NullS
latch _ NullS = NullS
latch (s:-ss) (r:-rr) = q
  where q = (s && not r) :- switch3 rr false ss true q

rateLim :: (Num a, Ord a) => (a, a) -> Signal a -> Signal a
rateLim _ NullS = NullS
rateLim (minRate, maxRate) (s:-ss) = out
  where out = s :- switch3 (ss - out \> top) (out + top)
                    (ss - out \< negate botton) (out - botton) ss
        top = constant maxRate
        botton = constant minRate


---------------------------------------------
--------------- Library tests ---------------
---------------------------------------------

x = sin (signal [0.0, 0.001..])
dx = delta false x / constant 0.001
y = cos (signal [0.0, 0.001..])
z = takeS 200000 (y - dx)
e = maximum $ fromSignal (tailS z)
