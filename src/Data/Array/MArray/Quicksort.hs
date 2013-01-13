module Data.Array.MArray.Quicksort (
    qsort
    ) where

import Control.Monad
import Data.Array.IO

--import Debug.Trace

{-# INLINE qsort #-}
qsort :: (MArray a e m, Ord e, Show e) => a Int e -> m ()
qsort = introsort' ( -1 ) undefined

{-# INLINE introsort' #-}
introsort' :: (MArray a e m, Ord e, Show e) => Int -> (a Int e -> Int -> Int -> m ()) -> a Int e -> m ()
introsort' maxdepth altsort a = getBounds a >>= \(mn, mx) -> srt maxdepth mn mx
  where
    trc = flip const
    trcShow = flip const
    srt depthleft mn mx
        | (mn + 1 >= mx)  = return ()
        | otherwise = trcShow (mn,mx) $ do
                -- Select a pivot - median of 3:
                pL <- readArray a mn
                pR <- readArray a mx
                let d = (mn + mx) `div` 2
                pD <- readArray a d
                let (_, pidx) = median3 (pR, mx) (pD, d) (pL, mn)

                p <- swap d mn
                i <- split p (mn + 1) mx
                trcShow (i, p) $ swap i mn
                if depthleft == 0
                    then trc ("Maximum depth reached for " ++ show (mn,mx)) $ do
                        altsort a mn (i - 1)
                        altsort a (i + 1) mx
                    else do
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
                    if v >= p
                        then return i
                        else moveRight (i + 1)
                moveLeft j | i > j = return j
                           | otherwise = do
                    v <- readArray a j
                    if v <= p
                        then return j
                        else moveLeft (j - 1)

{-# INLINE median3 #-}
median3 :: Ord a => a -> a -> a -> a
median3 a b c
    | a > b     = sel a b
    | otherwise = sel b a
  where
    sel l s = case compare c l of
        GT -> l
        LT -> s
        _  -> c
