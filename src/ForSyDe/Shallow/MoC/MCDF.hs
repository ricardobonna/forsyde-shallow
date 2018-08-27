-----------------------------------------------------------------------------
-- |
-- Module      :  ForSyDe.Shallow.MoC.MCDF
-- Copyright   :  (c) Ricardo Bonna, KTH/ICT/ES, ForSyDe-Group
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ricardobonna@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Experimental lib. Further test needed
--
-----------------------------------------------------------------------------

module ForSyDe.Shallow.MoC.MCDF (
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delayMCDF,
  -- * Data dependent actors
  -- | Based on the process constructors in the MCDF-MoC, the
  -- MCDF-library provides Switches, Selects and Tunnels
  switchMCDF, selectMCDF, tunnelMCDF,
  -- Tests
  switchtest, selecttest, tunneltest
  ) where

import ForSyDe.Shallow.Core

infixr 5 +|+

-------------------------------------
--             --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--             --
-------------------------------------

-- | The process constructor 'delayMCDF' delays the signal n event
--   cycles by introducing n initial values at the beginning of the
--   output signal.
delayMCDF :: [a] -> Signal a -> Signal a
delayMCDF initial_tokens xs = signal initial_tokens +-+ xs

-------------------------------------
--              --
--      DATA DEPENDENT ACTORS      --
--              --
-------------------------------------

-- | The process constructor 'switchMCDF' implements a switch process with
--   n modes where each mode, selected by each event in the control singnal ct,
--   consumes a number of tokens defined by c.
switchMCDF :: Int -> Int -> Signal Int -> Signal a -> [Signal a]
switchMCDF n _ NullS _ = replicate n NullS
switchMCDF n c (mode:-cts) s
  | c <= 0 = error "switchMCDF: Token consumption/production must be a positive integer"
  | mode > n && mode < 1 = error "switchMCDF: Outside mode range"
  | not $ sufficient_tokens c s = replicate n NullS
  | otherwise = ((replicate (mode-1) NullS) ++ [consumed_tokens] ++ (replicate (n-mode) NullS))
                  +|+ switchMCDF n c cts (dropS c s)
  where consumed_tokens = takeS c s


-- | The process constructor 'selectMCDF' implements a select process with
--   n modes where each mode, selected by each event in the control singnal ct,
--   consumes a number of tokens defined by c.
selectMCDF :: Int -> Int -> Signal Int -> [Signal a] -> Signal a
selectMCDF _ _ NullS _ = NullS
selectMCDF n c (mode:-cts) s
  | length s /= n = error "selectMCDF: Mismatch between number of inputs and modes"
  | c <= 0 = error "selectMCDF: Token consumption/production must be a positive integer"
  | mode > n && mode < 1 = error "selectMCDF: Outside mode range"
  | not $ sufficient_tokens c sig = NullS
  | otherwise = consumed_tokens +-+ selectMCDF n c cts (take (mode-1) s ++ [dropS c sig] ++ drop (mode) s)
  where sig = s !! (mode-1)
        consumed_tokens = takeS c sig


-- | The process constructor 'tunnelMCDF' implements a tunnel process between
--   an actor from a mode m1 to a mode m2 with token consumption defined by c
--   and initial conditions given by the list v0.
tunnelMCDF :: Int -> (Int, Int) -> [a] -> Signal Int -> Signal a -> Signal a
tunnelMCDF c (m1, m2) v0 ct s
  | length v0 /= c = error "tunnelMCDF: Mismatch between the number of consumed tokens and initial tokens"
  | otherwise = out
  where (out, fb) = buffer c (m1, m2) ct s fb'
        fb' = delayMCDF v0 fb


-------------------------------------
--               --
--        HELPER FUNCTIONS         --
--               --
-------------------------------------

(+|+) :: [Signal a] -> [Signal a] -> [Signal a]
(+|+) [] s2 = s2
(+|+) s1 [] = s1
(+|+) (s1:s1s) (s2:s2s) = (s1 +-+ s2) : (s1s +|+ s2s)


sufficient_tokens :: (Num a, Eq a, Ord a) => a -> Signal t -> Bool
sufficient_tokens 0 _     = True
sufficient_tokens _ NullS = False
sufficient_tokens n (_:-xs)
 = if n < 0 then
     error "sufficient_tokens: n must not be negative"
   else
     sufficient_tokens (n-1) xs


buffer :: Int -> (Int, Int) -> Signal Int -> Signal a -> Signal a -> (Signal a, Signal a)
buffer _ _ NullS _ _ = (NullS, NullS)
buffer _ _ _ _ NullS = (NullS, NullS)
buffer c (m1, m2) (mode:-cts) s1 s2
  | mode == m1 = if sufficient_tokens c s1 then (s1m1, takeS c s1 +-+ s2m1)
                  else (NullS, NullS)
  | mode == m2 = (takeS c s2 +-+ s1m2, takeS c s2 +-+ s2m2)
  | otherwise = buffer c (m1, m2) cts s1 s2
  where (s1m1, s2m1) = buffer c (m1, m2) cts (dropS c s1) (dropS c s2)
        (s1m2, s2m2) = buffer c (m1, m2) cts s1 (dropS c s2)


--------------------------------------------
-- Tests
--------------------------------------------
ct = signal [1,2,1,3,1,3,2,1]
ct2 = signal [1,2,1,2,1,2,1,2]
inp1 = signal [1 .. 30]
inp2 = signal [31 .. 60]
inp3 = signal [61 .. 90]
switchtest = switchMCDF 3 2 ct inp1
selecttest = selectMCDF 3 2 ct [inp1, inp2, inp3]
tunneltest = tunnelMCDF 2 (0,1) [0,0] ct2 inp1
