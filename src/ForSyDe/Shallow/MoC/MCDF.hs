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
  (+|+),
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delayMCDF,
  -- * Data dependent actors
  -- | Based on the process constructors in the MCDF-MoC, the
  -- MCDF-library provides Switches, Selects and tunnels
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
--   consumes a number of tokens defined by the list tcp.
switchMCDF :: Int -> [Int] -> Signal Int -> Signal a -> [Signal a]
switchMCDF n tcp ct s
  | length tcp /= n = error "switchMCDF: Mismatch between number of modes and token consumption list"
  | minimum tcp <= 0 = error "switchMCDF: Token production must be a positive integer"
  | ct == NullS = replicate n NullS
  | mode >= n = error "switchMCDF: Outside mode range"
  | not $ sufficient_tokens c s = replicate n NullS
  | otherwise = ((replicate mode NullS) ++ [consumed_tokens] ++ (replicate (n-mode-1) NullS))
                  +|+ switchMCDF n tcp (tailS ct) (dropS c s)
  where mode = headS ct
        c = tcp !! mode
        consumed_tokens = takeS c s


-- | The process constructor 'selectMCDF' implements a select process with
--   n modes where each mode, selected by each event in the control singnal ct,
--   consumes a number of tokens defined by the list tcp.
selectMCDF :: Int -> [Int] -> Signal Int -> [Signal a] -> Signal a
selectMCDF n tcp ct s
  | length tcp /= n = error "selectMCDF: Mismatch between number of modes and token consumption list"
  | length s /= n = error "selectMCDF: Mismatch between number of inputs and modes"
  | minimum tcp <= 0 = error "selectMCDF: Token production must be a positive integer"
  | ct == NullS = NullS
  | mode >= n = error "selectMCDF: Outside mode range"
  | not $ sufficient_tokens c sig = NullS
  | otherwise = consumed_tokens +-+ selectMCDF n tcp (tailS ct) (take mode s ++ [dropS c sig] ++ drop (mode+1) s)
  where mode = headS ct
        c = tcp !! mode
        sig = s !! mode
        consumed_tokens = takeS c sig

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
ct = signal [0,1,0,2,0,2,1,0]
ct2 = signal [0,1,0,1,0,1,1,0]
inp1 = signal [1 .. 30]
inp2 = signal [31 .. 60]
inp3 = signal [61 .. 90]
switchtest = switchMCDF 3 [1, 2, 3] ct inp1
selecttest = selectMCDF 3 [1, 2, 3] ct [inp1, inp2, inp3]
tunneltest = tunnelMCDF 2 (0,1) [0,0] ct2 inp1
