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
  Action,
  -- * Sequential Process Constructors
  -- | Sequential process constructors are used for processes that
  -- have a state. One of the input parameters is the initial state.
  delay,
  -- * Reconfigurable Processes
  -- | Process constructors for an experimental MoC called ReconDF
  actor11ReconDF, actor21ReconDF
  -- Tests
  -- , output
  ) where

import ForSyDe.Shallow.Core


data Action a = Fire | Reconfig a



-------------------------------------
--             --
-- SEQUENTIAL PROCESS CONSTRUCTORS --
--             --
-------------------------------------

-- | The process constructor 'delay' delays the signal n event
--   cycles by introducing n initial values at the beginning of the
--   output signal.
delay :: [a] -> Signal a -> Signal a
delay initial_tokens xs = signal initial_tokens +-+ xs


------------------------------------------------------------------------
--
-- Reconfig Processes
--
------------------------------------------------------------------------

-- > ReconDF actors with only one output

-- | The process constructor 'actor11ReconDF' ...
actor11ReconDF :: (Int, Int, [a] -> [b])                  -- ^ Initial config
               -> Signal (Action (Int, Int, [a] -> [b]))  -- ^ Reconfig Action signal
               -> Signal a                                -- ^ Input
               -> Signal b                                -- ^ Output
actor11ReconDF c0 ct x = outs
  where (cl1, outs) = unzipReconDF $ mapReconDF ct cl x
        cl = delay [c0] cl1

-- | The process constructor 'actor21ReconDF' ...
actor21ReconDF :: ((Int, Int), Int, [a] -> [b] -> [c])                  -- ^ Initial config
               -> Signal (Action ((Int, Int), Int, [a] -> [b] -> [c]))  -- ^ Reconfig Action signal
               -> Signal a -> Signal b                                  -- ^ Inputs
               -> Signal c                                              -- ^ Output
actor21ReconDF c0 ct as bs = outs
  where (cl1, outs) = unzipReconDF $ zipWithReconDF ct cl as bs
        cl = delay [c0] cl1


------------------------------------------------------------------------
-- COMBINATIONAL PROCESS CONSTRUCTORS (not exported)
------------------------------------------------------------------------

-- | The process constructor 'mapReconDF' ...
mapReconDF :: Signal (Action (Int, Int, [a] -> [b])) -> Signal (Int, Int, [a] -> [b]) -> Signal a
           -> Signal ((Int, Int, [a] -> [b]), [b])
mapReconDF NullS _ _ = NullS
mapReconDF _ NullS _ = NullS
mapReconDF (Reconfig ct:-cts) cl xs = signal [(ct, [])] +-+ mapReconDF cts (tailS cl) xs
mapReconDF (Fire:-cts) cl xs
    | c < 0 = error "mapReconDF: Number of consumed tokens must be a non-negative integer"
    | not $ sufficient_tokens c xs  = NullS
    | otherwise = if length produced_tokens == p then
                    signal [((c,p,f) ,produced_tokens)] +-+ mapReconDF cts (tailS cl) (dropS c xs)
                  else
                    error "mapReconDF: Function does not produce correct number of tokens"
    where (c, p, f) = headS cl
          consumed_tokens = fromSignal $ takeS c xs
          produced_tokens = f consumed_tokens


-- | The process constructor 'zipWithReconDF' ...
zipWithReconDF :: Signal (Action ((Int, Int), Int, [a] -> [b] -> [c]))
               -> Signal ((Int, Int), Int, [a] -> [b] -> [c]) -> Signal a -> Signal b
               -> Signal (((Int, Int), Int, [a] -> [b] -> [c]), [c])
zipWithReconDF NullS _ _ _ = NullS
zipWithReconDF _ NullS _ _ = NullS
zipWithReconDF (Reconfig ct:-cts) cl as bs = signal [(ct, [])] +-+ zipWithReconDF cts (tailS cl) as bs
zipWithReconDF (Fire:-cts) cl as bs
    | c1 < 0 || c2 < 0  = error "zipWithReconDF: Number of consumed tokens must be a non-negative integer"
    | (not $ sufficient_tokens c1 as)
      || (not $ sufficient_tokens c2 bs) = NullS
    | otherwise = if length produced_tokens == p then
                    signal [(((c1, c2),p,f) ,produced_tokens)] +-+ zipWithReconDF cts (tailS cl)
                      (dropS c1 as) (dropS c2 bs)
                  else
                    error "mapReconDF: Function does not produce correct number of tokens"
    where ((c1, c2), p, f) = headS cl
          consumed_tokens_as = fromSignal $ takeS c1 as
          consumed_tokens_bs = fromSignal $ takeS c2 bs
          produced_tokens = f consumed_tokens_as consumed_tokens_bs



------------------------------------------------------------------------
-- unzipSADF Processes (not exported)
------------------------------------------------------------------------

unzipReconDF :: Signal (a, [b]) -> (Signal a, Signal b)
unzipReconDF NullS = (NullS, NullS)
unzipReconDF ((s1, s2) :- ss) = (signal [s1] +-+ sr1, signal s2 +-+ sr2)
  where (sr1, sr2) = unzipReconDF ss


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







-- Tests

c0 = (1,1,\[a] -> [a])
c1 = (1,1,\[a] -> [2*a])
c2 = (1,2,\[a] -> [a, 3*a])

reconfigSig = signal [Fire, Fire, Reconfig c1, Fire, Reconfig c2, Fire, Fire]
inputSig = signal [1..10]

output = actor11ReconDF c0 reconfigSig inputSig
