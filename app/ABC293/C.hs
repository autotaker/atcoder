{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module ABC293.C where

import           Control.Monad.Except
import           Data.Char                  (isSpace)
import           Data.Function              ((&))
import           Data.IntMap                (mapEither)
import qualified Data.IntSet                as IS
import           Data.List                  (foldl', intersperse)
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

type Matrix a = ((Int,Int), V.Vector (UV.Vector a))

getMatrix :: M (Matrix Int)
getMatrix = do
    (h, w) <- getIntPair
    (,) (h,w) <$> V.generateM h (const (getIntList w))

main :: IO ()
main = handleError $ do
    cells <- getMatrix
    liftIO $ print $ solve cells

solve :: Matrix Int -> Int
solve ((h, w), cells) = go IS.empty (0, 0)
    where
        go acc (i, j)
            | IS.member a acc = 0
            | (i, j) == (h -1, w -1) = 1
            | i == h - 1 = go acc' (i, j + 1)
            | j == w - 1 = go acc' (i + 1, j)
            | otherwise = go acc' (i, j + 1) + go acc' (i + 1, j)
            where
                a = cells V.! i UV.! j
                acc' = IS.insert a acc
