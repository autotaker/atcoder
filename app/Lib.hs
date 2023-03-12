{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase       #-}
module Lib where

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
