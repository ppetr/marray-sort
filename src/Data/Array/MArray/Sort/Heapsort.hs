{-# OPTIONS_GHC -O2 #-}
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

data Dirn = L | R | N

{-# INLINE heapsort #-}
heapsort :: (Integral i, Ix i, Ord e, MArray a e m) => a i e -> i -> i -> m ()
-- | Sort the elements of the given 'MArray' in increasing order.
heapsort a mn mx = do
    heapify
    extract
  where
    getN = mx - mn + 1

    {-# INLINE left #-}
    left n i = 2 * i + 1  

    {-# INLINE right #-}
    right n i = 2 * i + 2

    {-# INLINE isLeaf #-}
    isLeaf n i = left n i >= n

    -- XXX Returns True when isLeaf is, so
    -- use with caution.
    {-# INLINE isEdge #-}
    isEdge n i = right n i >= n

    {-# INLINE atIndex #-}
    atIndex i = readArray a (i + mn)

    {-# INLINE exch #-}
    exch ix1 ix2 = do
      v1 <- readArray a (ix1 + mn)
      v2 <- readArray a (ix2 + mn)
      writeArray a (ix1 + mn) v2
      writeArray a (ix2 + mn) v1

    {-# INLINE downHeap #-}
    downHeap n i = do
      let il = left n i
      let ir = right n i
      c <- atIndex i
      let exchWith
            | isLeaf n i = return N
            | isEdge n i = do
                l <- atIndex il
                return (if c >= l then N else L)
            | otherwise = do
                l <- atIndex il
                r <- atIndex ir
                return (if c >= l && c >= r  
                          then N
                          else if l >= r 
                            then L
                            else R)
      x <- exchWith
      case x of
        L -> do
          exch i il
          downHeap n il
        R -> do
          exch i ir
          downHeap n ir
        N -> return ()

    {-# INLINE heapify #-}
    heapify = do
      let n = getN
      let heapifyRoots i
            | isLeaf n i = 
              return ()
            | isEdge n i = do
                heapifyRoots (left n i)
                downHeap n i
            | otherwise = do
                heapifyRoots (left n i)
                heapifyRoots (right n i)
                downHeap n i
      heapifyRoots 0
        

    {-# INLINE extract #-}
    extract = extractRoot (getN - 1)
      where
        extractRoot k | k <= 0    = return ()
                      | otherwise = do
            exch k 0
            downHeap k 0
