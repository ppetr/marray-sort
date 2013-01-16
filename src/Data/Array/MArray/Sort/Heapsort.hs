-- Created by refactoring Bart Massey's heapsort package: http://hackage.haskell.org/package/heapsort
--
-- Copyright  2010 Bart Massey
-- Heapsort in Haskell
--
-- Copyright  2012 Petr Pudlak
-- Refactoring and adding speed
-- optimizations.

-- | This module provides heapsort for an 'MArray'
-- with indices of 'Integral' type.  See any good
-- algorithms text for a description of heapsort.
-- Heapsort is O(n lg n) comparisons and exchanges
-- in the worst case, and is reasonably efficient.
module Data.Array.MArray.Sort.Heapsort (heapsort, heapsort')
where

import Control.Monad
import Data.Array.MArray

-- Internally, heap indices are transformed
-- to be zero-based.

data NodeType = Inner | Leaf | Edge

{-# INLINE heapsort #-}
heapsort :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort a = getBounds a >>= uncurry (heapsort' a)

{-# INLINE heapsort' #-}
heapsort' :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> i -> i -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort' a mn mx = do
    heapify
    extract
  where
    getN = mx - mn + 1

    {-# INLINE right #-}
    right i = 2 * i + 2

    {-# INLINE nodeType #-}
    nodeType n i = case compare (right i) n of
        GT  -> Leaf
        EQ  -> Edge
        LT  -> Inner

    {-# INLINE atIndex #-}
    atIndex i = readArray a (i + mn)

    {-# INLINE atIndexW #-}
    atIndexW i = writeArray a (i + mn)

    {-# INLINE downHeap #-}
    downHeap n j = do
        c <- atIndex j
        dh c j
      where
        -- Propagate @c@ down the heap. We only move elements up as we walk on
        -- the nodes, and when we're finished, we write @c@ to the last node.
        -- This way we save half of read/writes, compared to swapping elements.
        dh c = dhStep
          where
            dhStep i = case nodeType n i of
                Leaf -> stop
                Edge -> do
                    l <- atIndex il
                    if c < l
                      then atIndexW i l >> atIndexW il c
                      else stop
                Inner -> do
                    l <- atIndex il
                    r <- atIndex ir
                    if c >= l && c >= r
                      then stop
                      else if l >= r
                        then atIndexW i l >> dhStep il
                        else atIndexW i r >> dhStep ir
              where
                {-# INLINE stop #-}
                stop = when (i /= j) (atIndexW i c)
                ir = right i
                il = ir - 1

    {-# INLINE heapify #-}
    heapify = heapifyRoots (n - 1) -- optimize: use 2^k such that k is maximal that 2^k < n
      where
        n = getN
        heapifyRoots i | i < 0  = return ()
                       | otherwise = downHeap n i >> heapifyRoots (i - 1)


    {-# INLINE exch #-}
    exch ix1 ix2 | ix1 == ix2   = return ()
                 | otherwise    = do
      let i = ix1 + mn
          j = ix2 + mn
      v1 <- readArray a i
      v2 <- readArray a j
      writeArray a j v1
      writeArray a i v2

    {-# INLINE extract #-}
    extract = extractRoot (getN - 1)
      where
        extractRoot k | k <= 0    = return ()
                      | otherwise = do
            exch k 0
            downHeap k 0
            extractRoot (k - 1)
