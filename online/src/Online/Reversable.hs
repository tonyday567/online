{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | online statistics based on a moving average
module Online.Reversable
  ( Reversable(..)
  , ReversableDecay(..)
  , Deaverager(..)
  , online'
  , deonline
  , avRev
  , absRev
  , sqRev
  ) where

import Control.Category
import Control.Foldl (Fold(..))
import NumHask.Prelude hiding ((.))
import Online.Averages
-- import Data.Functor.Contravariant

-- $setup
--
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
-- >>> import NumHask.Prelude
-- >>> import qualified Control.Foldl as L
-- >>> let n = 100
-- >>> let r = 0.9

-- | deaverage reverses the averaging computation
--
-- > id == drop 1 . L.scan (deaverage rev) . drop 1 . L.scan (average rev)
--
-- >>> let xs = [1..n] :: [Float]
-- >>> let avs = drop 1 $ L.scan (online' (avRev r)) xs
-- >>> let xs' = drop 1 $ L.scan (deonline (avRev r)) avs
-- >>> (1e-3) > (L.fold (absma 1) $ zipWith (-) xs xs')
-- True
--
-- >>> let absavs = drop 1 $ L.scan (online' (absRev r)) xs
-- >>> let absxs' = drop 1 $ L.scan (deonline (absRev r)) absavs
-- >>> (1e-3) > (L.fold (absma 1) $ zipWith (-) xs absxs')
-- True
--
-- >>> let sqavs = drop 1 $ L.scan (online' (absRev r)) xs
-- >>> let sqxs' = drop 1 $ L.scan (deonline (absRev r)) sqavs
-- >>> (1e-3) > (L.fold (absma 1) $ zipWith (-) xs sqxs')
-- True
--
-- | Reversable consists of a function and it's isomorphic reverse, so
--
-- > reverseF . forwardF == id
--
data Reversable a b = Reversable
  { forwardF :: a -> b
  , reverseF :: b -> a
  }

instance Category Reversable where
  id = Reversable id id
  (.) (Reversable f r) (Reversable f' r') = Reversable (f . f') (r' . r)

rev :: Reversable a b -> Reversable b a
rev (Reversable f r) = Reversable r f

-- | a ReversableDecay includes a Reversable function and a decay function, together with it's reverse
data ReversableDecay a b = ReversableDecay { revF :: Reversable a b, revDecay :: Reversable b b}

-- | a Fold given a Reversable premap and decay function
online' :: (Field b) => ReversableDecay a b -> Fold a b
online' (ReversableDecay (Reversable f _) (Reversable g _)) = Fold step begin extract
  where
    begin = Averager (zero, zero)
    step (Averager (s, c)) a = Averager (g $ s + f a, g $ c + one)
    extract (Averager (s,c))= s / c

-- | isomorphic reversal of an Averager
data Deaverager a b = Deaverager
  { deA :: a
  , deC :: b
  , deS :: a
  }

-- | algebraic reversal of an online' fold
deonline :: (AdditiveGroup b, Multiplicative b) => ReversableDecay a b -> Fold b a
deonline (ReversableDecay (Reversable _ r) (Reversable gf gr)) = Fold step begin extract
  where
    begin = Deaverager zero zero zero
    step (Deaverager _ c s) sc =
      let c' = gf (c + one)
          s' = c' * sc
          a' = (gr s' - s)
      in Deaverager a' c' s'
    extract (Deaverager a _ _) = r a

-- | reversable ma
--
-- > online' (avRev r) = ma r
avRev :: (Field a) => a -> ReversableDecay a a
avRev r = ReversableDecay (Reversable id id) (Reversable (*r) (/r))

data AbsTuple a = AbsTuple a a deriving (Show)

instance (AdditiveGroup a, Multiplicative a, Signed a) => AdditiveMagma (AbsTuple a) where plus (AbsTuple a s) (AbsTuple a' s') = let x = (a*s) `plus` (a'*s') in AbsTuple (abs x) (sign x)
instance (AdditiveGroup a, Multiplicative a, Signed a) => AdditiveAssociative (AbsTuple a)
instance (AdditiveGroup a, Multiplicative a, Signed a) => AdditiveInvertible (AbsTuple a) where negate (AbsTuple a s) = AbsTuple a (negate s)
instance (AdditiveGroup a, Multiplicative a, Signed a) => AdditiveUnital (AbsTuple a) where zero = AbsTuple zero one
instance (AdditiveGroup a, Multiplicative a, Signed a) => AdditiveCommutative (AbsTuple a)
instance (AdditiveGroup a, Multiplicative a, Signed a) => Additive (AbsTuple a)
instance (AdditiveGroup a, Multiplicative a, Signed a) => AdditiveGroup (AbsTuple a)
instance (MultiplicativeMagma a) => MultiplicativeMagma (AbsTuple a) where times (AbsTuple a s) (AbsTuple a' s') = AbsTuple (a `times` a') (s `times` s')
instance (MultiplicativeMagma a) => MultiplicativeAssociative (AbsTuple a)
instance (MultiplicativeUnital a) => MultiplicativeUnital (AbsTuple a) where one = AbsTuple one one
instance (MultiplicativeInvertible a) => MultiplicativeInvertible (AbsTuple a) where recip(AbsTuple a s) = AbsTuple (recip a) s
instance (MultiplicativeCommutative a) => MultiplicativeCommutative (AbsTuple a)
instance (Multiplicative a) => Multiplicative (AbsTuple a)
instance (MultiplicativeGroup a) => MultiplicativeGroup (AbsTuple a)
instance (Signed a, Multiplicative a, AdditiveGroup a) => Distribution (AbsTuple a)
instance (Signed a, Multiplicative a, AdditiveGroup a) => Semiring (AbsTuple a)
instance (Signed a, Multiplicative a, Ring a) => Ring (AbsTuple a)
instance (Signed a, Field a) => Field (AbsTuple a)


absDecay :: (MultiplicativeGroup a) => a -> Reversable (AbsTuple a) (AbsTuple a)
absDecay r =
  Reversable
  (\(AbsTuple a s) -> (AbsTuple (a*r) s))
  (\(AbsTuple a s) -> AbsTuple (a/r) s)

-- | unary reversables

abs' :: (Signed a, Multiplicative a) => Reversable a (AbsTuple a)
abs' = Reversable (\x -> AbsTuple (abs x) (sign x)) (\(AbsTuple a s) -> s*a)

sq' :: (ExpField a, Signed a) => Reversable a (AbsTuple a)
sq' = Reversable (\x -> AbsTuple (x * x) (sign x)) (\(AbsTuple a s) -> s * sqrt a)

-- | binary reversables

-- op' :: (Additive a) => Reversable (a, b) c -> Reversable a b -> Reversable a b

plus'' :: (AdditiveGroup a) => Reversable (a,a) (a,a)
plus'' = Reversable (\(a,b) -> (a+b,a)) (\(s,a) -> (a,s-a))

minus' :: (AdditiveGroup a) => Reversable (a,a) (a,a)
minus' = rev plus''

times' :: (MultiplicativeGroup a) => Reversable (a,a) (a,a)
times' = Reversable (\(a,b) -> (a*b,a)) (\(s,a) -> (a,s/a))

div' :: (MultiplicativeGroup a) => Reversable (a,a) (a,a)
div' = rev times'

tup :: Reversable a b -> Reversable c d -> Reversable (a,c) (b,d)
tup (Reversable fa rb) (Reversable fc rd) = Reversable (\(a,c) -> (fa a, fc c)) (\(b,d) -> (rb b, rd d))


-- | reversable absma
--
-- > fst <$> (online' (absRev r)) = absma r
--
absRev :: (Field a, Signed a) => a -> ReversableDecay a (AbsTuple a)
absRev r =
  ReversableDecay
  abs'
  (absDecay r)

-- | reversable sqma
--
-- > fst <$> (online' (sqRev r)) = sqma r
--
sqRev :: (ExpField a, Signed a) => a -> ReversableDecay a (AbsTuple a)
sqRev r =
  ReversableDecay
  sq'
  (absDecay r)

data ReversableFold a b = ReversableFold { forwardFold :: Fold a b, reverseFold :: Fold b a}

mkFold :: (Field b) => ReversableDecay a b -> ReversableFold a b
mkFold rd = ReversableFold
  (online' rd)
  (deonline rd)

-- instance Functor (ReversableFold a) where
--   fmap g (ReversableFold f r) = ReversableFold (fmap g f) (premap g r)

add :: Reversable e (a,c) -> Reversable f (b,d) -> Reversable a b -> Reversable c d -> Reversable e f
add (Reversable fe re) (Reversable ff rf) (Reversable fa rb) (Reversable fc rd) =
  Reversable
  ((\(a,c) -> rf (fa a, fc c)) . fe)
  ((\(b,d) -> re (rb b, rd d)) . ff)

maabs :: Reversable a b -> Reversable c d -> Reversable (a,c) (b,d)
maabs (Reversable fab rba) (Reversable fcd rdc) =
  Reversable (\(a,c) -> (fab a, fcd c)) (\(b,d) -> (rba b, rdc d))
