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
  actor11ReconDF, actor21ReconDF, actor31ReconDF, actor41ReconDF, actor51ReconDF,
  actor12ReconDF, actor22ReconDF, actor32ReconDF, actor42ReconDF, actor52ReconDF,
  actor13ReconDF, actor23ReconDF, actor33ReconDF, actor43ReconDF, actor53ReconDF,
  actor14ReconDF, actor24ReconDF, actor34ReconDF, actor44ReconDF, actor54ReconDF,
  actor15ReconDF, actor25ReconDF, actor35ReconDF, actor45ReconDF, actor55ReconDF
  -- Tests
  ,output
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
  where (cl1, outs) = unzipCtrl $ mapReconDF ct cl x
        cl = delay [c0] cl1

-- | The process constructor 'actor21ReconDF' ...
actor21ReconDF :: ((Int, Int), Int, [a] -> [b] -> [c])
               -> Signal (Action ((Int, Int), Int, [a] -> [b] -> [c]))
               -> Signal a -> Signal b
               -> Signal c
actor21ReconDF c0 ct as bs = outs
  where (cl1, outs) = unzipCtrl $ zipWithReconDF ct cl as bs
        cl = delay [c0] cl1

-- | The process constructor 'actor31ReconDF' ...
actor31ReconDF :: ((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d])
               -> Signal (Action ((Int, Int, Int), Int, [a] -> [b] -> [c] -> [d]))
               -> Signal a -> Signal b -> Signal c
               -> Signal d
actor31ReconDF c0 ct as bs cs = outs
  where (cl1, outs) = unzipCtrl $ zipWith3ReconDF ct cl as bs cs
        cl = delay [c0] cl1

-- | The process constructor 'actor31ReconDF' ...
actor41ReconDF :: ((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e])
               -> Signal (Action ((Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e]))
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> Signal e
actor41ReconDF c0 ct as bs cs ds = outs
  where (cl1, outs) = unzipCtrl $ zipWith4ReconDF ct cl as bs cs ds
        cl = delay [c0] cl1

-- | The process constructor 'actor31ReconDF' ...
actor51ReconDF :: ((Int, Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e] -> [f])
               -> Signal (Action ((Int, Int, Int, Int, Int), Int, [a] -> [b] -> [c] -> [d] -> [e] -> [f]))
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> Signal f
actor51ReconDF c0 ct as bs cs ds es = outs
  where (cl1, outs) = unzipCtrl $ zipWith5ReconDF ct cl as bs cs ds es
        cl = delay [c0] cl1


-- > ReconDF actors with two outputs

-- | The process constructor 'actor12ReconDF' ...
actor12ReconDF :: (Int, (Int, Int), [a] -> ([b], [c]))
               -> Signal (Action (Int, (Int, Int), [a] -> ([b], [c])))
               -> Signal a
               -> (Signal b, Signal c)
actor12ReconDF c0 ct x = (out1, out2)
  where (cl1, out1, out2) = unzipCtrl2 $ mapReconDF (funPackS ct) cl x
        cl = delay [funPack c0] cl1

-- | The process constructor 'actor22ReconDF' ...
actor22ReconDF :: ((Int, Int), (Int, Int), [a] -> [b] -> ([c], [d]))
               -> Signal (Action ((Int, Int), (Int, Int), [a] -> [b] -> ([c], [d])))
               -> Signal a -> Signal b
               -> (Signal c, Signal d)
actor22ReconDF c0 ct as bs = (out1, out2)
  where (cl1, out1, out2) = unzipCtrl2 $ zipWithReconDF (funPack2S ct) cl as bs
        cl = delay [funPack2 c0] cl1

-- | The process constructor 'actor32ReconDF' ...
actor32ReconDF :: ((Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> ([d], [e]))
               -> Signal (Action ((Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> ([d], [e])))
               -> Signal a -> Signal b -> Signal c
               -> (Signal d, Signal e)
actor32ReconDF c0 ct as bs cs = (out1, out2)
  where (cl1, out1, out2) = unzipCtrl2 $ zipWith3ReconDF (funPack3S ct) cl as bs cs
        cl = delay [funPack3 c0] cl1

-- | The process constructor 'actor42ReconDF' ...
actor42ReconDF :: ((Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f]))
               -> Signal (Action ((Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f])))
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal e, Signal f)
actor42ReconDF c0 ct as bs cs ds = (out1, out2)
  where (cl1, out1, out2) = unzipCtrl2 $ zipWith4ReconDF (funPack4S ct) cl as bs cs ds
        cl = delay [funPack4 c0] cl1

-- | The process constructor 'actor52ReconDF' ...
actor52ReconDF :: ((Int, Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g]))
               -> Signal (Action ((Int, Int, Int, Int, Int), (Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g])))
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal f, Signal g)
actor52ReconDF c0 ct as bs cs ds es = (out1, out2)
  where (cl1, out1, out2) = unzipCtrl2 $ zipWith5ReconDF (funPack5S ct) cl as bs cs ds es
        cl = delay [funPack5 c0] cl1


-- > ReconDF actors with three outputs

-- | The process constructor 'actor13ReconDF' ...
actor13ReconDF :: (Int, (Int, Int, Int), [a] -> ([b], [c], [d]))
               -> Signal (Action (Int, (Int, Int, Int), [a] -> ([b], [c], [d])))
               -> Signal a
               -> (Signal b, Signal c, Signal d)
actor13ReconDF c0 ct x = (out1, out2, out3)
  where (cl1, out1, out2, out3) = unzipCtrl3 $ mapReconDF (funPackS ct) cl x
        cl = delay [funPack c0] cl1

-- | The process constructor 'actor23ReconDF' ...
actor23ReconDF :: ((Int, Int), (Int, Int, Int), [a] -> [b] -> ([c], [d], [e]))
               -> Signal (Action ((Int, Int), (Int, Int, Int), [a] -> [b] -> ([c], [d], [e])))
               -> Signal a -> Signal b
               -> (Signal c, Signal d, Signal e)
actor23ReconDF c0 ct as bs = (out1, out2, out3)
  where (cl1, out1, out2, out3) = unzipCtrl3 $ zipWithReconDF (funPack2S ct) cl as bs
        cl = delay [funPack2 c0] cl1

-- | The process constructor 'actor33ReconDF' ...
actor33ReconDF :: ((Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f]))
               -> Signal (Action ((Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f])))
               -> Signal a -> Signal b -> Signal c
               -> (Signal d, Signal e, Signal f)
actor33ReconDF c0 ct as bs cs = (out1, out2, out3)
  where (cl1, out1, out2, out3) = unzipCtrl3 $ zipWith3ReconDF (funPack3S ct) cl as bs cs
        cl = delay [funPack3 c0] cl1

-- | The process constructor 'actor43ReconDF' ...
actor43ReconDF :: ((Int, Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f], [g]))
               -> Signal (Action ((Int, Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f], [g])))
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal e, Signal f, Signal g)
actor43ReconDF c0 ct as bs cs ds = (out1, out2, out3)
  where (cl1, out1, out2, out3) = unzipCtrl3 $ zipWith4ReconDF (funPack4S ct) cl as bs cs ds
        cl = delay [funPack4 c0] cl1

-- | The process constructor 'actor53ReconDF' ...
actor53ReconDF :: ((Int, Int, Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h]))
               -> Signal (Action ((Int, Int, Int, Int, Int), (Int, Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h])))
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal f, Signal g, Signal h)
actor53ReconDF c0 ct as bs cs ds es = (out1, out2, out3)
  where (cl1, out1, out2, out3) = unzipCtrl3 $ zipWith5ReconDF (funPack5S ct) cl as bs cs ds es
        cl = delay [funPack5 c0] cl1


-- > ReconDF actors with four outputs

-- | The process constructor 'actor14ReconDF' ...
actor14ReconDF :: (Int, (Int, Int, Int, Int), [a] -> ([b], [c], [d], [e]))
               -> Signal (Action (Int, (Int, Int, Int, Int), [a] -> ([b], [c], [d], [e])))
               -> Signal a
               -> (Signal b, Signal c, Signal d, Signal e)
actor14ReconDF c0 ct x = (out1, out2, out3, out4)
  where (cl1, out1, out2, out3, out4) = unzipCtrl4 $ mapReconDF (funPackS ct) cl x
        cl = delay [funPack c0] cl1

-- | The process constructor 'actor24ReconDF' ...
actor24ReconDF :: ((Int, Int), (Int, Int, Int, Int), [a] -> [b] -> ([c], [d], [e], [f]))
               -> Signal (Action ((Int, Int), (Int, Int, Int, Int), [a] -> [b] -> ([c], [d], [e], [f])))
               -> Signal a -> Signal b
               -> (Signal c, Signal d, Signal e, Signal f)
actor24ReconDF c0 ct as bs = (out1, out2, out3, out4)
  where (cl1, out1, out2, out3, out4) = unzipCtrl4 $ zipWithReconDF (funPack2S ct) cl as bs
        cl = delay [funPack2 c0] cl1

-- | The process constructor 'actor34ReconDF' ...
actor34ReconDF :: ((Int, Int, Int), (Int, Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f], [g]))
               -> Signal (Action ((Int, Int, Int), (Int, Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f], [g])))
               -> Signal a -> Signal b -> Signal c
               -> (Signal d, Signal e, Signal f, Signal g)
actor34ReconDF c0 ct as bs cs = (out1, out2, out3, out4)
  where (cl1, out1, out2, out3, out4) = unzipCtrl4 $ zipWith3ReconDF (funPack3S ct) cl as bs cs
        cl = delay [funPack3 c0] cl1

-- | The process constructor 'actor44ReconDF' ...
actor44ReconDF :: ((Int, Int, Int, Int), (Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f], [g], [h]))
               -> Signal (Action ((Int, Int, Int, Int), (Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f], [g], [h])))
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal e, Signal f, Signal g, Signal h)
actor44ReconDF c0 ct as bs cs ds = (out1, out2, out3, out4)
  where (cl1, out1, out2, out3, out4) = unzipCtrl4 $ zipWith4ReconDF (funPack4S ct) cl as bs cs ds
        cl = delay [funPack4 c0] cl1

-- | The process constructor 'actor54ReconDF' ...
actor54ReconDF :: ((Int, Int, Int, Int, Int), (Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h], [i]))
               -> Signal (Action ((Int, Int, Int, Int, Int), (Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h], [i])))
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal f, Signal g, Signal h, Signal i)
actor54ReconDF c0 ct as bs cs ds es = (out1, out2, out3, out4)
  where (cl1, out1, out2, out3, out4) = unzipCtrl4 $ zipWith5ReconDF (funPack5S ct) cl as bs cs ds es
        cl = delay [funPack5 c0] cl1


-- > ReconDF actors with five outputs

-- | The process constructor 'actor15ReconDF' ...
actor15ReconDF :: (Int, (Int, Int, Int, Int, Int), [a] -> ([b], [c], [d], [e], [f]))
               -> Signal (Action (Int, (Int, Int, Int, Int, Int), [a] -> ([b], [c], [d], [e], [f])))
               -> Signal a
               -> (Signal b, Signal c, Signal d, Signal e, Signal f)
actor15ReconDF c0 ct x = (out1, out2, out3, out4, out5)
  where (cl1, out1, out2, out3, out4, out5) = unzipCtrl5 $ mapReconDF (funPackS ct) cl x
        cl = delay [funPack c0] cl1

-- | The process constructor 'actor25ReconDF' ...
actor25ReconDF :: ((Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> ([c], [d], [e], [f], [g]))
               -> Signal (Action ((Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> ([c], [d], [e], [f], [g])))
               -> Signal a -> Signal b
               -> (Signal c, Signal d, Signal e, Signal f, Signal g)
actor25ReconDF c0 ct as bs = (out1, out2, out3, out4, out5)
  where (cl1, out1, out2, out3, out4, out5) = unzipCtrl5 $ zipWithReconDF (funPack2S ct) cl as bs
        cl = delay [funPack2 c0] cl1

-- | The process constructor 'actor35ReconDF' ...
actor35ReconDF :: ((Int, Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f], [g], [h]))
               -> Signal (Action ((Int, Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> [c] -> ([d], [e], [f], [g], [h])))
               -> Signal a -> Signal b -> Signal c
               -> (Signal d, Signal e, Signal f, Signal g, Signal h)
actor35ReconDF c0 ct as bs cs = (out1, out2, out3, out4, out5)
  where (cl1, out1, out2, out3, out4, out5) = unzipCtrl5 $ zipWith3ReconDF (funPack3S ct) cl as bs cs
        cl = delay [funPack3 c0] cl1

-- | The process constructor 'actor45ReconDF' ...
actor45ReconDF :: ((Int, Int, Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f], [g], [h], [i]))
               -> Signal (Action ((Int, Int, Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> ([e], [f], [g], [h], [i])))
               -> Signal a -> Signal b -> Signal c -> Signal d
               -> (Signal e, Signal f, Signal g, Signal h, Signal i)
actor45ReconDF c0 ct as bs cs ds = (out1, out2, out3, out4, out5)
  where (cl1, out1, out2, out3, out4, out5) = unzipCtrl5 $ zipWith4ReconDF (funPack4S ct) cl as bs cs ds
        cl = delay [funPack4 c0] cl1

-- | The process constructor 'actor55ReconDF' ...
actor55ReconDF :: ((Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h], [i], [j]))
               -> Signal (Action ((Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int), [a] -> [b] -> [c] -> [d] -> [e] -> ([f], [g], [h], [i], [j])))
               -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
               -> (Signal f, Signal g, Signal h, Signal i, Signal j)
actor55ReconDF c0 ct as bs cs ds es = (out1, out2, out3, out4, out5)
  where (cl1, out1, out2, out3, out4, out5) = unzipCtrl5 $ zipWith5ReconDF (funPack5S ct) cl as bs cs ds es
        cl = delay [funPack5 c0] cl1


------------------------------------------------------------------------
-- COMBINATIONAL PROCESS CONSTRUCTORS (not exported)
------------------------------------------------------------------------

-- | The process constructor 'mapReconDF' ...
mapReconDF :: Signal (Action (Int, p, [a] -> [b])) -> Signal (Int, p, [a] -> [b]) -> Signal a
           -> Signal ((Int, p, [a] -> [b]), [b])
mapReconDF NullS _ _ = NullS
mapReconDF _ NullS _ = NullS
mapReconDF (Reconfig ct:-cts) (_:-cls) xs = signal [(ct, [])] +-+ mapReconDF cts cls xs
mapReconDF (Fire:-cts) (cl:-cls) xs
    | c < 0 = error "mapReconDF: Number of consumed tokens must be a non-negative integer"
    | not $ sufficient_tokens c xs  = NullS
    | otherwise = signal [(cl, produced_tokens)] +-+ mapReconDF cts cls (dropS c xs)
    where (c, _, f) = cl
          consumed_tokens = fromSignal $ takeS c xs
          produced_tokens = f consumed_tokens


-- | The process constructor 'zipWithReconDF' ...
zipWithReconDF :: Signal (Action ((Int, Int), p, [a] -> [b] -> [c]))
               -> Signal ((Int, Int), p, [a] -> [b] -> [c]) -> Signal a -> Signal b
               -> Signal (((Int, Int), p, [a] -> [b] -> [c]), [c])
zipWithReconDF NullS _ _ _ = NullS
zipWithReconDF _ NullS _ _ = NullS
zipWithReconDF (Reconfig ct:-cts) (_:-cls) as bs = signal [(ct, [])] +-+ zipWithReconDF cts cls as bs
zipWithReconDF (Fire:-cts) (cl:-cls) as bs
    | c1 < 0 || c2 < 0  = error "zipWithReconDF: Number of consumed tokens must be a non-negative integer"
    | (not $ sufficient_tokens c1 as)
      || (not $ sufficient_tokens c2 bs) = NullS
    | otherwise = signal [(cl, produced_tokens)] +-+ zipWithReconDF cts cls
                      (dropS c1 as) (dropS c2 bs)
    where ((c1, c2), _, f) = cl
          consumed_tokens_as = fromSignal $ takeS c1 as
          consumed_tokens_bs = fromSignal $ takeS c2 bs
          produced_tokens = f consumed_tokens_as consumed_tokens_bs


-- | The process constructor 'zipWith3ReconDF' ...
zipWith3ReconDF :: Signal (Action ((Int, Int, Int), p, [a] -> [b] -> [c] -> [d]))
                -> Signal ((Int, Int, Int), p, [a] -> [b] -> [c] -> [d])
                -> Signal a -> Signal b -> Signal c
                -> Signal (((Int, Int, Int), p, [a] -> [b] -> [c] -> [d]), [d])
zipWith3ReconDF NullS _ _ _ _ = NullS
zipWith3ReconDF _ NullS _ _ _ = NullS
zipWith3ReconDF (Reconfig ct:-cts) (_:-cls) as bs cs = signal [(ct, [])] +-+ zipWith3ReconDF cts cls as bs cs
zipWith3ReconDF (Fire:-cts) (cl:-cls) as bs cs
    | c1 < 0 || c2 < 0 || c3 < 0 = error "zipWith3ReconDF: Number of consumed tokens must be a non-negative integer"
    | (not $ sufficient_tokens c1 as)
      || (not $ sufficient_tokens c2 bs)
      || (not $ sufficient_tokens c3 cs) = NullS
    | otherwise = signal [(cl, produced_tokens)] +-+ zipWith3ReconDF cts cls
                      (dropS c1 as) (dropS c2 bs) (dropS c3 cs)
    where ((c1, c2, c3), _, f) = cl
          consumed_tokens_as = fromSignal $ takeS c1 as
          consumed_tokens_bs = fromSignal $ takeS c2 bs
          consumed_tokens_cs = fromSignal $ takeS c3 cs
          produced_tokens = f consumed_tokens_as consumed_tokens_bs consumed_tokens_cs


-- | The process constructor 'zipWith4ReconDF' ...
zipWith4ReconDF :: Signal (Action ((Int, Int, Int, Int), p, [a] -> [b] -> [c] -> [d] -> [e]))
                -> Signal ((Int, Int, Int, Int), p, [a] -> [b] -> [c] -> [d] -> [e])
                -> Signal a -> Signal b -> Signal c -> Signal d
                -> Signal (((Int, Int, Int, Int), p, [a] -> [b] -> [c] -> [d] -> [e]), [e])
zipWith4ReconDF NullS _ _ _ _ _ = NullS
zipWith4ReconDF _ NullS _ _ _ _ = NullS
zipWith4ReconDF (Reconfig ct:-cts) (_:-cls) as bs cs ds = signal [(ct, [])] +-+ zipWith4ReconDF cts cls as bs cs ds
zipWith4ReconDF (Fire:-cts) (cl:-cls) as bs cs ds
    | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0
      = error "zipWith4ReconDF: Number of consumed tokens must be a non-negative integer"
    | (not $ sufficient_tokens c1 as)
      || (not $ sufficient_tokens c2 bs)
      || (not $ sufficient_tokens c3 cs)
      || (not $ sufficient_tokens c4 ds) = NullS
    | otherwise = signal [(cl, produced_tokens)] +-+ zipWith4ReconDF cts cls
                      (dropS c1 as) (dropS c2 bs) (dropS c3 cs) (dropS c4 ds)
    where ((c1, c2, c3, c4), _, f) = cl
          consumed_tokens_as = fromSignal $ takeS c1 as
          consumed_tokens_bs = fromSignal $ takeS c2 bs
          consumed_tokens_cs = fromSignal $ takeS c3 cs
          consumed_tokens_ds = fromSignal $ takeS c4 ds
          produced_tokens = f consumed_tokens_as consumed_tokens_bs
                              consumed_tokens_cs consumed_tokens_ds


-- | The process constructor 'zipWith4ReconDF' ...
zipWith5ReconDF :: Signal (Action ((Int, Int, Int, Int, Int), p, [a] -> [b] -> [c] -> [d] -> [e] -> [f]))
                -> Signal ((Int, Int, Int, Int, Int), p, [a] -> [b] -> [c] -> [d] -> [e] -> [f])
                -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
                -> Signal (((Int, Int, Int, Int, Int), p, [a] -> [b] -> [c] -> [d] -> [e] -> [f]), [f])
zipWith5ReconDF NullS _ _ _ _ _ _ = NullS
zipWith5ReconDF _ NullS _ _ _ _ _ = NullS
zipWith5ReconDF (Reconfig ct:-cts) (_:-cls) as bs cs ds es = signal [(ct, [])] +-+
                                                        zipWith5ReconDF cts cls as bs cs ds es
zipWith5ReconDF (Fire:-cts) (cl:-cls) as bs cs ds es
    | c1 < 0 || c2 < 0 || c3 < 0 || c4 < 0 || c5 < 0
      = error "zipWith5ReconDF: Number of consumed tokens must be a non-negative integer"
    | (not $ sufficient_tokens c1 as)
      || (not $ sufficient_tokens c2 bs)
      || (not $ sufficient_tokens c3 cs)
      || (not $ sufficient_tokens c4 ds)
      || (not $ sufficient_tokens c5 es) = NullS
    | otherwise = signal [(cl, produced_tokens)] +-+ zipWith5ReconDF cts cls
                      (dropS c1 as) (dropS c2 bs) (dropS c3 cs) (dropS c4 ds) (dropS c5 es)
    where ((c1, c2, c3, c4, c5), _, f) = cl
          consumed_tokens_as = fromSignal $ takeS c1 as
          consumed_tokens_bs = fromSignal $ takeS c2 bs
          consumed_tokens_cs = fromSignal $ takeS c3 cs
          consumed_tokens_ds = fromSignal $ takeS c4 ds
          consumed_tokens_es = fromSignal $ takeS c5 es
          produced_tokens = f consumed_tokens_as consumed_tokens_bs
                              consumed_tokens_cs consumed_tokens_ds consumed_tokens_es



------------------------------------------------------------------------
-- unzipSADF Processes (not exported)
------------------------------------------------------------------------

unzipCtrl :: Signal ((con, Int, fun), [a]) -> (Signal (con, Int, fun), Signal a)
unzipCtrl NullS = (NullS, NullS)
unzipCtrl ((s1, s2) :- ss)
  | length s2 == 0 = (signal [s1] +-+ sr1, sr2)
  | length s2 /= p = error "unzipCtrl: Wrong number of produced tokens"
  | otherwise = (signal [s1] +-+ sr1, signal s2 +-+ sr2)
  where (sr1, sr2) = unzipCtrl ss
        (_,p,_) = s1


unzipCtrl2 :: Signal ((con, (Int, Int), fun), [([a], [b])])
           -> (Signal (con, (Int, Int), fun), Signal a, Signal b)
unzipCtrl2 NullS = (NullS, NullS, NullS)
unzipCtrl2 ((s1, []) :- ss) = (signal [s1] +-+ sr1, sr2, sr3)
  where (sr1, sr2, sr3) = unzipCtrl2 ss
unzipCtrl2 ((s1, [(s2, s3)]) :- ss)
  | length s2 /= p1 || length s3 /= p2 = error "unzipCtrl2: Wrong number of produced tokens"
  | otherwise = (signal [s1] +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3)
  where (sr1, sr2, sr3) = unzipCtrl2 ss
        (_,(p1,p2),_) = s1


unzipCtrl3 :: Signal ((con, (Int, Int, Int), fun), [([a], [b], [c])])
           -> (Signal (con, (Int, Int, Int), fun), Signal a, Signal b, Signal c)
unzipCtrl3 NullS = (NullS, NullS, NullS, NullS)
unzipCtrl3 ((s1, []) :- ss) = (signal [s1] +-+ sr1, sr2, sr3, sr4)
  where (sr1, sr2, sr3, sr4) = unzipCtrl3 ss
unzipCtrl3 ((s1, [(s2, s3, s4)]) :- ss)
  | length s2 /= p1 || length s3 /= p2
    || length s4 /= p3 = error "unzipCtrl3: Wrong number of produced tokens"
  | otherwise = (signal [s1] +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3, signal s4 +-+ sr4)
  where (sr1, sr2, sr3, sr4) = unzipCtrl3 ss
        (_,(p1,p2,p3),_) = s1


unzipCtrl4 :: Signal ((con, (Int, Int, Int, Int), fun), [([a], [b], [c], [d])])
           -> (Signal (con, (Int, Int, Int, Int), fun), Signal a, Signal b, Signal c, Signal d)
unzipCtrl4 NullS = (NullS, NullS, NullS, NullS, NullS)
unzipCtrl4 ((s1, []) :- ss) = (signal [s1] +-+ sr1, sr2, sr3, sr4, sr5)
  where (sr1, sr2, sr3, sr4, sr5) = unzipCtrl4 ss
unzipCtrl4 ((s1, [(s2, s3, s4, s5)]) :- ss)
  | length s2 /= p1 || length s3 /= p2 || length s4 /= p3
    || length s5 /= p4 = error "unzipCtrl4: Wrong number of produced tokens"
  | otherwise = (signal [s1] +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3,
                 signal s4 +-+ sr4, signal s5 +-+ sr5)
  where (sr1, sr2, sr3, sr4, sr5) = unzipCtrl4 ss
        (_,(p1,p2,p3,p4),_) = s1


unzipCtrl5 :: Signal ((con, (Int, Int, Int, Int, Int), fun), [([a], [b], [c], [d], [e])])
           -> (Signal (con, (Int, Int, Int, Int, Int), fun), Signal a, Signal b, Signal c, Signal d, Signal e)
unzipCtrl5 NullS = (NullS, NullS, NullS, NullS, NullS, NullS)
unzipCtrl5 ((s1, []) :- ss) = (signal [s1] +-+ sr1, sr2, sr3, sr4, sr5, sr6)
  where (sr1, sr2, sr3, sr4, sr5, sr6) = unzipCtrl5 ss
unzipCtrl5 ((s1, [(s2, s3, s4, s5, s6)]) :- ss)
  | length s2 /= p1 || length s3 /= p2 || length s4 /= p3
    || length s5 /= p4 || length s6 /= p5 = error "unzipCtrl5: Wrong number of produced tokens"
  | otherwise = (signal [s1] +-+ sr1, signal s2 +-+ sr2, signal s3 +-+ sr3,
                 signal s4 +-+ sr4, signal s5 +-+ sr5, signal s6 +-+ sr6)
  where (sr1, sr2, sr3, sr4, sr5, sr6) = unzipCtrl5 ss
        (_,(p1,p2,p3,p4, p5),_) = s1



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


funPack :: (con, prod, [a] -> y) -> (con, prod, [a] -> [y])
funPack (c, p, f) = (c, p, \x -> [f x])

funPack2 :: (con, prod, [a] -> [b] -> y) -> (con, prod, [a] -> [b] -> [y])
funPack2 (c, p, f) = (c, p, \x1 x2 -> [f x1 x2])

funPack3 :: (con, prod, [a] -> [b] -> [c] -> y) -> (con, prod, [a] -> [b] -> [c] -> [y])
funPack3 (c, p, f) = (c, p, \x1 x2 x3 -> [f x1 x2 x3])

funPack4 :: (con, prod, [a] -> [b] -> [c] -> [d] -> y)
         -> (con, prod, [a] -> [b] -> [c] -> [d] -> [y])
funPack4 (c, p, f) = (c, p, \x1 x2 x3 x4 -> [f x1 x2 x3 x4])

funPack5 :: (con, prod, [a] -> [b] -> [c] -> [d] -> [e] -> y)
         -> (con, prod, [a] -> [b] -> [c] -> [d] -> [e] -> [y])
funPack5 (c, p, f) = (c, p, \x1 x2 x3 x4 x5 -> [f x1 x2 x3 x4 x5])


funPackS :: Signal (Action (con, prod, [a] -> y))
         -> Signal (Action (con, prod, [a] -> [y]))
funPackS NullS = NullS
funPackS (Fire :- ss) = signal [Fire] +-+ funPackS ss
funPackS (Reconfig s :- ss) = signal [Reconfig (funPack s)] +-+ funPackS ss

funPack2S :: Signal (Action (con, prod, [a] -> [b] -> y))
          -> Signal (Action (con, prod, [a] -> [b] -> [y]))
funPack2S NullS = NullS
funPack2S (Fire :- ss) = signal [Fire] +-+ funPack2S ss
funPack2S (Reconfig s :- ss) = signal [Reconfig (funPack2 s)] +-+ funPack2S ss

funPack3S :: Signal (Action (con, prod, [a] -> [b] -> [c] -> y))
          -> Signal (Action (con, prod, [a] -> [b] -> [c] -> [y]))
funPack3S NullS = NullS
funPack3S (Fire :- ss) = signal [Fire] +-+ funPack3S ss
funPack3S (Reconfig s :- ss) = signal [Reconfig (funPack3 s)] +-+ funPack3S ss

funPack4S :: Signal (Action (con, prod, [a] -> [b] -> [c] -> [d] -> y))
          -> Signal (Action (con, prod, [a] -> [b] -> [c] -> [d] -> [y]))
funPack4S NullS = NullS
funPack4S (Fire :- ss) = signal [Fire] +-+ funPack4S ss
funPack4S (Reconfig s :- ss) = signal [Reconfig (funPack4 s)] +-+ funPack4S ss

funPack5S :: Signal (Action (con, prod, [a] -> [b] -> [c] -> [d] -> [e] -> y))
          -> Signal (Action (con, prod, [a] -> [b] -> [c] -> [d] -> [e] -> [y]))
funPack5S NullS = NullS
funPack5S (Fire :- ss) = signal [Fire] +-+ funPack5S ss
funPack5S (Reconfig s :- ss) = signal [Reconfig (funPack5 s)] +-+ funPack5S ss



-- Tests

c0 = (1,(1,1),\[a] -> ([a], [(-a)]))
c1 = (1,(1,0),\[a] -> ([2*a], []))
c2 = (1,(0,2),\[a] -> ([], [a, 3*a]))

reconfigSig = signal [Fire, Fire, Reconfig c1, Fire, Reconfig c2, Fire, Fire]
inputSig = signal [1..10]

output = actor12ReconDF c0 reconfigSig inputSig
-- expected answer: {1,2,6,4,12,5,15}
