{-# OPTIONS_GHC -O2 #-}
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

    {-# INLINE left #-}
    left i = 2 * i + 1  

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
    exch ix1 ix2 = do
      v1 <- readArray a (ix1 + mn)
      v2 <- readArray a (ix2 + mn)
      writeArray a (ix1 + mn) v2
      writeArray a (ix2 + mn) v1

    {-# INLINE downHeap #-}
    downHeap n i = dh i
      where
        dh i = case nodeType n i of
            Leaf -> stop
            Edge -> do
                c <- atIndex i
                l <- atIndex il
                if c >= l then stop else stepL
            Inner -> do
                c <- atIndex i
                l <- atIndex il
                r <- atIndex ir
                if c >= l && c >= r  
                  then stop
                  else if l >= r 
                    then stepL
                    else stepR
          where
            stop = return ()
            stepL = exch i il >> dh il
            stepR = exch i ir >> dh ir
            il = left i
            ir = right i

    {-# INLINE heapify #-}
    heapify = heapifyRoots 0
      where
          n = getN
          heapifyRoots i = case nodeType n i of
            Leaf -> return ()
            Edge -> do
                heapifyRoots (left i)
                downHeap n i
            Inner -> do
                heapifyRoots (left i)
                heapifyRoots (right i)
                downHeap n i


    {-# INLINE extract #-}
    extract = extractRoot (getN - 1)
      where
        extractRoot k | k <= 0    = return ()
                      | otherwise = do
            exch k 0
            downHeap k 0
