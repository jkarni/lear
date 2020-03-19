{-# LANGUAGE AllowAmbiguousTypes #-}

module Lear.Internal.Lens where

import Control.Lens
import Data.Bifunctor
import Data.Generics.Product
import Lear.Internal.Type

liftLens :: Lens' (p, a) b -> Lear p a b
liftLens l = Lear $ \p a -> ((p, a) ^. l, \b' -> first const $ (p, a) & l .~ b')

liftLens' :: Lens' a b -> Lear p a b
liftLens' l = liftLens (_2 . l)

-- | A lifted version of `the`.
look :: forall (sel :: k) a b p. HasAny sel a a b b => Lear p a b
look = liftLens' (the @sel)

-- | Take a param using a lens.
withParam :: Lear p a b -> Lens' p' p -> Lear p' a b
withParam (Lear f) ls = Lear $ \p a ->
  let (b, lin) = f (p ^. ls) a
   in ( b,
        \b' ->
          let (fp, a) = lin b'
           in ((& ls %~ fp), a)
      )

param :: Lear p a p
param = Lear $ \p a -> (p, \p' -> (const p', a))

input :: Lear p a a
input = Lear $ \p a -> (a, (const p,))
