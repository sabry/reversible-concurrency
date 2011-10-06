-- At any communication event, and of course choice points, we
-- checkpoint. The checkpoint should save not only our state and
-- continutation, but a copy of all our channels at that time, and our
-- timestamp.
--
-- When we return, we set our time and the channels time to the maximum
-- time. We need to set the time correctly on all our channels,
-- differently for send and receive. 
--
-- We then wait on all channels, specifically on the time stamp. If
-- our partner's time increases to maximum, we stop waiting on that
-- channel. 
--
-- If a channels time decreases, our partner has backtracked, and we
-- should go back to that channels time. Note that since the partner has
-- restored the channels, all channels should be in a consistent state,
-- and this should propigate through the network.
--
-- If we need to backtrack, we pop continuations until we find the
-- time we're looking for. Then, we restore all our channels.
--
-- XXX: Race condition. What if a partner sends/recvs as we set the
-- channel time backwards? Since they are synchronous, the partner can't
-- detect that we've backtracked. Perhaps we can ignore this for now,
-- and add a form of concurrency to check for backtracking while we
-- compute. This will solve the other issue of only backtracking upon
-- return.
