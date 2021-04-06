{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lear.Internal.Lens where

import Control.Lens
import Data.Bifunctor
import Data.Generics.Product
import Data.Monoid
import GHC.OverloadedLabels
import GHC.Types
import Lear.Internal.Type

liftLens' :: c p => Lens' (p, a) b -> Lear c a b
liftLens' l = Lear $ \p a -> ((p, a) ^. l, \b' -> (p, a) & l .~ b')

liftLens :: forall c a b. c () => Lens' a b -> Lear c a b
liftLens l = liftLens' (_2 . l :: Lens' ((), a) b)

-- | A lifted version of `the`.
look :: forall (sel :: Symbol) a b c. (c (), HasField sel a a b b) => Lear c a b
look = liftLens (field @sel)

instance (c (), HasField name a a b b) => IsLabel name (Lear c a b) where
  fromLabel = liftLens (field @name)

param :: (c p) => Lear c a p
param = liftLens' _1
{-



-- | Take a param using a lens.
withParam :: Lear p a b -> Lens' p' p -> Lear p' a b
withParam (Lear f) ls = Lear $ \p a ->
  let (b, lin) = f (p ^. ls) a
   in ( b,
        \b' ->
          let (fp, a) = lin b'
           in ((& ls %~ fp), a)
      )
-}
