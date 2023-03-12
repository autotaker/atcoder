{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs        #-}
module Lib.Mod(Mod(..), SomeMod, KnownMod) where
import           Data.Proxy   (Proxy (..))
import           GHC.Base     (Int (I#), magicDict, remInt#)
import           GHC.TypeLits (KnownNat, Nat, natVal)

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
