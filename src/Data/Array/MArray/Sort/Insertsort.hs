module Data.Array.MArray.Sort.Insertsort
    ( insertsort
    , insertsort'
    ) where

import Control.Monad
import Data.Array.IO


{-# INLINE insertsort #-}
insertsort :: (MArray a e m, Ord e) => a Int e -> m ()
insertsort a = getBounds a >>= uncurry (insertsort' (return ()) a)

{-# INLINE insertsort' #-}
insertsort' :: (MArray a e m, Ord e) => m () -> a Int e -> Int -> Int -> m ()
insertsort' cmpaction a mn mx = srt (mn + 1)
  where
    srt i = when (i <= mx) $ do
                v <- readArray a i
                j <- hole v i
                when (i /= j) (writeArray a j v)
                srt (i + 1)
    hole v = hole'
      where
        hole' i | i == mn     = return i
                | otherwise   = do
                    let i' = i - 1
                    u <- readArray a i'
                    cmpaction
                    if v < u
                      then writeArray a i u >> hole' i'
                      else return i
