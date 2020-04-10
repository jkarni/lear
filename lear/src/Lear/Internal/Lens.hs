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

liftLens' :: Lens' (p, a) b -> Lear p a b
liftLens' l = Lear $ \p a -> ((p, a) ^. l, \b' -> bimap (Endo . const) (Endo . const) $ (p, a) & l .~ b')

liftLens :: Lens' a b -> Lear p a b
liftLens l = liftLens' (_2 . l)

-- | A lifted version of `the`.
look :: forall (sel :: Symbol) a b p. HasField sel a a b b => Lear p a b
look = liftLens (field @sel)

instance HasField name a a b b => IsLabel name (Lear p a b) where
  fromLabel = liftLens (field @name)

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

param :: Lear p a p
param = Lear $ \p a -> (p, \p' -> (Endo $ const p', mempty))

input :: Lear p a a
input = Lear $ \p a -> (a, \a' -> (mempty, Endo $ const a'))
