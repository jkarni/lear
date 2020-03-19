{-# LANGUAGE AllowAmbiguousTypes #-}

module Lear.Internal.Lens where

import Control.Lens
import Data.Bifunctor
import Data.Generics.Product
import Lear.Internal.Type

liftLens :: Lens (p, a) (p, a) b b -> Lear p a b
liftLens l = Lear $ \p a -> ((p, a) ^. l, \b' -> first const $ (p, a) & l .~ b')

liftLens' :: Lens a a b b -> Lear p a b
liftLens' l = liftLens (_2 . l)

look :: forall sel a b p. HasAny sel a a b b => Lear p a b
look = liftLens' (the @sel)

param :: Lear p a p
param = Lear $ \p a -> (p, \p' -> (const p', a))

input :: Lear p a a
input = Lear $ \p a -> (a, (const p,))
