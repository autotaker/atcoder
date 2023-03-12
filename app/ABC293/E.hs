{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module ABC293.E(main) where

import           Control.Monad.Except
import           Data.Char                  (isSpace)
import           Data.Function              ((&))
import           Data.IntMap                (mapEither)
import qualified Data.IntSet                as IS
import           Data.List                  (foldl', intersperse)
import           Data.Proxy
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.IO          as L
import qualified Data.Text.Read             as T
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV
import           GHC.Base                   (Int (I#), magicDict, remInt#)
import           GHC.Stack                  (CallStack, HasCallStack, callStack,
                                             prettyCallStack)
import           GHC.TypeLits               (KnownNat, Nat, natVal)

newtype Mod (n :: Nat) = Mod { unMod :: Int }
    deriving(Eq, Ord)

newtype SInt (n :: Nat) = SInt Int

class KnownMod n where
    intSing :: SInt n

data WrapN a b where
  WrapN :: (KnownMod a => Proxy a -> b) -> WrapN a b

data SomeMod where
    SomeMod :: KnownMod n => Proxy n -> SomeMod

someModVal :: Int -> Maybe SomeMod
someModVal n
    | n <= 1 = Nothing
    | otherwise =
        Just $ magicDict (WrapN SomeMod) (SInt n) Proxy

instance KnownMod n => Show (Mod n) where
    {-# INLINE show #-}
    show n = showM (unMod n) (modulo n)
    {-# INLINE showsPrec #-}
    showsPrec prec n =
        showParen (prec >= 7) $
            shows (unMod n)
            . showString " % "
            . shows (modulo n)

{-# INLINE CONLIKE modVal #-}
modVal :: forall n. KnownMod n => Int
modVal = case intSing @n of SInt i -> i

{-# INLINE showM #-}
showM :: Int -> Int -> String
showM n m = show n ++ " % " ++ show m

{-# INLINE modulo #-}
modulo :: forall n. KnownMod n => Mod n -> Int
modulo _ = modVal @n

{-# INLINE unsafeRem #-}
unsafeRem :: Int -> Int -> Int
unsafeRem (I# x) (I# y) = I# (remInt# x y)

toMod :: Proxy n -> Int -> Mod n
toMod _ = Mod

instance KnownMod n => Num (Mod n) where
    {-# INLINE (+) #-}
    x + y = Mod ((unMod x + unMod y) `unsafeRem` modulo x)
    {-# INLINE (*) #-}
    x * y = Mod ((unMod x * unMod y) `unsafeRem` modulo x)
    {-# INLINE negate #-}
    negate :: KnownMod n => Mod n -> Mod n
    negate x = Mod (modulo x - unMod x)
    {-# INLINE (-) #-}
    x - y = Mod ((unMod x + (modulo x - unMod y)) `unsafeRem` modulo x)
    {-# INLINE fromInteger #-}
    fromInteger x =
        case fromInteger x `unsafeRem` m of
            v | v < 0 -> Mod (m + v)
              | otherwise -> Mod v
        where m = modVal @n
    {-# INLINE abs #-}
    abs = id
    {-# INLINE signum #-}
    signum = const (Mod 1)


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

getIntTriple :: HasCallStack => M (Int, Int, Int)
getIntTriple = do
    s <- liftIO $ T.getLine
    (n1, s) <- parseInt callStack s
    (n2, s) <- parseInt callStack (T.dropWhile isSpace s)
    (n3, _) <- parseInt callStack (T.dropWhile isSpace s)
    pure (n1, n2, n3)


main :: IO ()
main = handleError $ do
    (a, x, m) <- getIntTriple
    liftIO $ print $ solve a x m

solve :: Int -> Int -> Int -> Int
solve a x m =
    case someModVal m of
        Just (SomeMod proxy) -> solve' (toMod proxy a) x
        Nothing              -> 0 -- mod is 1

solve' :: KnownMod n => Mod n -> Int -> Int
solve' a x = unMod $ sa * sb + rem_a
    where
    sqrt_x = floor (sqrt $ fromIntegral x)
    x_ = sqrt_x * sqrt_x
    ga = a ^ sqrt_x
    sb = expSum ga sqrt_x
    sa = expSum a sqrt_x
    rem_a = a ^ x_ * expSum a (x - x_)

-- sum [ a ^ i | i <- [0..x-1]]
expSum :: KnownMod n => Mod n -> Int -> Mod n
expSum a = go 0
    where
        go acc 0 = acc
        go acc x = go (acc * a + 1) (x-1)
