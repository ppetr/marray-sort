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
module Data.Array.MArray.Sort.Heapsort (heapsort)
where

import Control.Monad
import Data.Array.MArray

-- Internally, heap indices are transformed
-- to be zero-based.

data NodeType = Inner | Leaf | Edge

{-# INLINE heapsort #-}
heapsort :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> i -> i -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort a mn mx = do
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

    {-# INLINE exch #-}
    exch ix1 ix2 | ix1 == ix2   = return ()
                 | otherwise    = do
      let i = ix1 + mn
          j = ix2 + mn
      v1 <- readArray a i
      v2 <- readArray a j
      writeArray a j v1
      writeArray a i v2

    {-# INLINE downHeap #-}
    downHeap n = dh
      where
        dh i = case nodeType n i of
            Leaf -> stop
            Edge -> do
                c <- atIndex i
                l <- atIndex il
                if c < l then exch i il else stop
            Inner -> do
                c <- atIndex i
                l <- atIndex il
                r <- atIndex ir
                if c >= l && c >= r  
                  then stop
                  else if l >= r 
                    then exch i il >> dh il
                    else exch i ir >> dh ir
          where
            stop = return ()
            ir = right i
            il = ir - 1

    {-# INLINE heapify #-}
    heapify = heapifyRoots (n - 1) -- optimize: use 2^k such that k is maximal that 2^k < n
      where
        n = getN
        heapifyRoots i | i < 0  = return ()
                       | otherwise = downHeap n i >> heapifyRoots (i - 1)


    {-# INLINE extract #-}
    extract = extractRoot (getN - 1)
      where
        extractRoot k | k <= 0    = return ()
                      | otherwise = do
            exch k 0
            downHeap k 0
            extractRoot (k - 1)
