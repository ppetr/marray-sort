{-# OPTIONS_GHC -O2 #-}
{- -O2 provides small, but noticable benefit for qsort. -}
module Data.Array.MArray.Sort.Introsort
    ( qsort
    , introsort
    -- internal
    , introsort'
    ) where

import Control.Monad
import Data.Array.IO
import Data.Array.MArray.Sort.Heapsort
import Data.Array.MArray.Sort.Insertsort

insertsortLimit :: Int
insertsortLimit = 32

depthCoeficient :: Double
depthCoeficient = 1.5 / log 2

{-# INLINE qsort #-}
qsort :: (MArray a e m, Ord e) => a Int e -> m ()
qsort = introsort' (return ()) ( -1 )

{-# INLINE maxQSortDepth #-}
maxQSortDepth :: (MArray a e m) => a Int e -> m Int
maxQSortDepth arr = do
    (mn, mx) <- getBounds arr
    return $ ceiling (depthCoeficient * log (fromIntegral $ mx - mn + 1))

{-# INLINE introsort #-}
introsort :: (MArray a e m, Ord e) => a Int e -> m ()
introsort arr = maxQSortDepth arr >>= \d -> introsort' (return ()) d arr

{-# INLINE introsort' #-}
introsort' :: (MArray a e m, Ord e) => m () -> Int -> a Int e -> m ()
introsort' cmpaction maxdepth a = getBounds a >>= uncurry (srt maxdepth)
  where
    srt depthleft mn mx
        | depthleft == 0             = heapsort a mn mx
        | mn + insertsortLimit > mx  = insertsort' cmpaction a mn mx
        | otherwise = do
                -- Select a pivot - median of 3:
                pL <- readArray a mn
                pR <- readArray a mx
                let d = (mn + mx) `div` 2
                pD <- readArray a d
                cmpaction
                let (_, pidx) = median3 (pR, mx) (pD, d) (pL, mn)

                p <- swap d mn
                i <- split p (mn + 1) mx
                swap i mn

                let depthleft' = depthleft - 1
                -- Sort the smaller part first. Hopefully, tail
                -- recursion will catch on the larger part and so we'll
                -- have guaranteed that even in the worst case, we'll
                -- have at most (log n) calls on the stack.
                if i < d then do
                    srt depthleft' mn (i - 1)
                    srt depthleft' (i + 1) mx
                 else do
                    srt depthleft' (i + 1) mx
                    srt depthleft' mn (i - 1)
      where
        {-# INLINE swap #-}
        swap i j = do
            u <- readArray a i
            when (i /= j) $ do
                v <- readArray a j
                writeArray a i v
                writeArray a j u
            return u
        {-# INLINE split #-}
        split p = split'
          where 
            split' i j = do
              i' <- moveRight i
              j' <- moveLeft j
              if i' < j'
                then swap i' j' >> split' (i' + 1) (j' - 1)
                else return j'
              where
                moveRight i | i > j = return i
                            | otherwise = do
                    v <- readArray a i
                    cmpaction
                    if v >= p
                        then return i
                        else moveRight (i + 1)
                moveLeft j | i > j = return j
                           | otherwise = do
                    v <- readArray a j
                    cmpaction
                    if v <= p
                        then return j
                        else moveLeft (j - 1)

{-# INLINE median3 #-}
median3 :: Ord a => a -> a -> a -> a
median3 a b c
    | a > b     = sel a b
    | otherwise = sel b a
  where
    sel l s | c > l     = l
            | c < s     = s
            | otherwise = c
