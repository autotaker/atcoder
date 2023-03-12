{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase       #-}
module ABC293.D where

import           Control.Monad               (when)
import           Control.Monad.Except        (ExceptT, MonadError (throwError),
                                              MonadIO (liftIO), runExceptT)
import           Control.Monad.Primitive     (PrimMonad (PrimState))
import           Control.Monad.ST            (ST, runST)
import           Data.Char                   (isSpace)
import           Data.Function               ((&))
import           Data.IntMap                 (mapEither)
import qualified Data.IntSet                 as IS
import           Data.List                   (foldl', intersperse)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy.Builder      as Builder
import qualified Data.Text.Lazy.Builder.Int  as Builder
import qualified Data.Text.Lazy.IO           as L
import qualified Data.Text.Read              as T
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           GHC.Stack                   (CallStack, HasCallStack,
                                              callStack, prettyCallStack)

data UnionFind s = UnionFind {
    rank   :: MUV.MVector s Int,
    parent :: MUV.MVector s Int
}

create :: PrimMonad m => Int -> m (UnionFind (PrimState m))
create n = do
    rank <- MUV.replicate n 0
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

type M a = ExceptT (CallStack, String) IO a

parseInt :: CallStack -> T.Text -> M (Int, T.Text)
parseInt cs s =
    case T.decimal s of
        Left err -> throwError (cs, err)
        Right a  -> pure a

getInt :: HasCallStack => M Int
getInt = do
    s <- liftIO T.getLine
    fst <$> parseInt callStack s

getIntPair :: HasCallStack => M (Int, Int)
getIntPair = do
    s <- liftIO T.getLine
    (n1, s) <- parseInt callStack s
    (n2, s) <- parseInt callStack (T.dropWhile isSpace s)
    pure (n1, n2)

getQuery :: HasCallStack => M (Int, Color, Int, Color)
getQuery = do
    s <- liftIO T.getLine
    (n1, s) <- parseInt callStack s
    (c1, s) <- parseColor callStack (T.dropWhile isSpace s)
    (n2, s) <- parseInt callStack (T.dropWhile isSpace s)
    (c2, s) <- parseColor callStack (T.dropWhile isSpace s)
    pure (n1 - 1, c1, n2 - 1, c2)

type Color = Char
parseColor :: CallStack -> T.Text -> M (Color, T.Text)
parseColor cs t
    | T.null t = throwError (cs, "Expecting Color but EOL")
    | T.head t == 'R' = pure ('R', T.tail t)
    | T.head t == 'B' = pure ('B', T.tail t)
    | otherwise = throwError (cs, "Expectiong Color but found " ++ show (T.head t))

getIntList :: HasCallStack => Int -> M (UV.Vector Int)
getIntList n = do
    line <- liftIO T.getLine
    UV.unfoldrNM n f line
  where
    f s = do
        (n, s) <- parseInt callStack s
        pure $ Just (n, T.dropWhile isSpace s)

printIntList :: UV.Vector Int -> IO ()
printIntList xs =
    xs & UV.toList
       & foldr (\x acc -> Builder.decimal x <> Builder.singleton ' ' <> acc) mempty
       & Builder.toLazyText
       & L.putStrLn

handleError :: M () -> IO ()
handleError action =
    runExceptT action >>= \case
        Left (cs, err) -> do
            putStrLn $ "Error: " ++ err
            putStrLn $ prettyCallStack cs
        Right a -> pure a


main :: IO ()
main = handleError $ do
    (n, m) <- getIntPair
    qs <- UV.generateM m (const getQuery)
    let (x, y) = solve n qs
    liftIO $ putStrLn $ show x ++ " " ++ show y


solve :: Int -> UV.Vector (Int, Color, Int, Color) -> (Int, Int)
solve n queries = runST doit
    where
    doit :: ST s (Int, Int)
    doit = do
        t <- create n
        UV.foldM' (\(x,y) (n1,c1,n2,c2) -> do
            r1 <- find t n1
            r2 <- find t n2
            union t n1 n2
            if r1 == r2 then
                pure (x+1, y-1)
            else
                pure (x, y -1)
            ) (0, n) queries

