-----------------------------------------------------------------------------
-- |
-- Module  :  ForSyDe.Shallow.MoC.Reconfig
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

module ForSyDe.Shallow.MoC.Reconfig (
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delay
  ) where

import ForSyDe.Shallow.Core


-------------------------------------
--             --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--             --
-------------------------------------

-- | The process constructor 'delayn' delays the signal n event
--   cycles by introducing n initial values at the beginning of the
--   output signal.
delay :: [a] -> Signal a -> Signal a
delay initial_tokens xs = signal initial_tokens +-+ xs


------------------------------------------------------------------------
--
-- Reconfig Processes
--
------------------------------------------------------------------------

-- | The process constructor 'mapSADF' takes a signal of scenarios
-- (tuples with the consumed and produced tokens as well as a function operating
-- on lists), and results in an SADF-process that takes an input signal and results
-- in an output signal
mapLSADF :: Signal (Maybe (Int, Int, [a] -> [b])) -> Signal (Int, Int, [a] -> [b]) -> Signal a
         -> (Signal (Int, Int, [a] -> [b]), Signal b)
mapLSADF NullS cl _ = (cl, NullS)
mapLSADF (Nothing:-cts) cl xs = NullS
  -- | c < 0 = error "kernelLSADF: Number of consumed tokens must be a non-negative integer"
  -- | not $ sufficient_tokens c xs  = NullS
  -- | otherwise = if length produced_tokens == p then
  --                 signal produced_tokens +-+ mapLSADF (tailS ct) (dropS c xs)
  --               else
  --                 error "kernelLSADF: Function does not produce correct number of tokens"
  -- where (c, p, f) = headS ct
  --       consumed_tokens = fromSignal $ takeS c xs
  --       produced_tokens = f consumed_tokens



------------------------------------------------------------------------
--
-- Helper functions (not exported!)
--
------------------------------------------------------------------------

sufficient_tokens :: (Num a, Eq a, Ord a) => a -> Signal t -> Bool
sufficient_tokens 0 _     = True
sufficient_tokens _ NullS = False
sufficient_tokens n (_:-xs)
 = if n < 0 then
     error "sufficient_tokens: n must not be negative"
   else
     sufficient_tokens (n-1) xs
