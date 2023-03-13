
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE LambdaCase       #-}
module ABC293.F where

import           Control.Monad.Except
import qualified Data.Bits                  as Bits
import           Data.Char                  (isSpace)
import           Data.Function              ((&))
import           Data.IntMap                (mapEither)
import qualified Data.IntSet                as IS
import           Data.List                  (foldl', intersperse)
import qualified Data.List                  as List
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.IO          as L
import qualified Data.Text.Read             as T
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV
import           GHC.Stack                  (CallStack, HasCallStack, callStack,
                                             prettyCallStack)

type M a = ExceptT (CallStack, String) IO a

parseInt :: CallStack -> T.Text -> M (Int, T.Text)
parseInt cs s =
    T.decimal s
        & (\case
            Left err -> Left (cs, err)
            Right a -> Right a)
        & liftEither

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
    t <- getInt
    replicateM_ t $ do
        n <- getInt
        liftIO $ print $ solve n

threshold :: Int
threshold = 4000

solve :: Int -> Int
solve n = length $ filter (checkB n) [2..threshold] ++ findLargeBs n

-- d > 9
-- b^(d-1) <= n < 2 * b^(d-1)
--
-- b <= sqrtN n (d-1) <= sqrtN n 8

findLargeBs :: Int -> [Int]
findLargeBs n = [ b | digits <- [2..63], b <- binSearch digits]
    where
    binSearch :: Int -> [Int]
    binSearch digits = do
        guard (b0 > threshold)
        guard (f b0 == n)
        pure b0
      where
        b0 = go l0 r0
        d = Bits.finiteBitSize digits - Bits.countLeadingZeros digits
        l0 = sqrtN (n `div` 2) (d - 1)
        r0 = sqrtN n (d - 1) + 1
        go l r | r - l <= 1 = r
               | f m < n = go m r
               | otherwise = go l m
               where m = (l + r) `div` 2
        -- a_0 + a_1 * b + a_2 * b^2 .. + a_{d-1} * d^{d-1}
        f b = go2 digits 1 0
            where
            go2 0 !c acc = acc
            go2 x !c acc = go2 (x `Bits.shiftR` 1) (c * b) (acc + c * (x Bits..&. 1))

sqrtN :: Int -> Int -> Int
sqrtN x 1 = x
sqrtN x d = floor (fromIntegral x ** (1/fromIntegral d))

checkB :: Int -> Int -> Bool
checkB n b = go n
    where
    go 0 = True
    go n | r <= 1 = go q
         | otherwise = False
            where (q,r) = divMod n b
