{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.UnionFind where

import           Control.Monad               (when)
import           Control.Monad.Primitive     (PrimMonad (PrimState))
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

data UnionFind s = UnionFind {
    rank   :: UV.MVector s Int,
    parent :: UV.MVector s Int
}

create :: PrimMonad m => Int -> m (UnionFind (PrimState m))
create n = do
    rank <- MUV.replicate n 0
    -- MUV.generateは0.12.3.0から
    parent <- UV.unsafeThaw (UV.generate n id)
    pure UnionFind{..}

{-# INLINE find #-}
find :: PrimMonad m => UnionFind (PrimState m) -> Int -> m Int
find UnionFind{parent} v = go v
    where
    go v = do
        p_v <- MUV.read parent v
        if p_v == v then
            pure v
        else do
            r_v <- go p_v
            MUV.write parent v r_v
            pure r_v

{-# INLINE union #-}
union :: PrimMonad m => UnionFind (PrimState m) -> Int -> Int -> m ()
union t@(UnionFind{..}) v w = do
    r_v <- find t v
    r_w <- find t w
    when (r_v /= r_w) $ do
        c_v <- MUV.read rank r_v
        c_w <- MUV.read rank r_w
        if c_v >= c_w then do
            MUV.write parent r_w r_v
            when (c_v == c_w) $
                MUV.write rank r_v (c_v + 1)
        else do
            MUV.write parent r_v r_w

