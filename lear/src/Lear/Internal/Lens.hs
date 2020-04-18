{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lear.Internal.Lens where

import Control.Lens
import Data.Bifunctor
import Data.Generics.Product
import Data.Monoid
import Data.Tuple (swap)
import GHC.OverloadedLabels
import GHC.Types
import Lear.Internal.Diff
import Lear.Internal.Type

liftLens' :: Lens' (p, a) b -> Lear p a b
liftLens' l = Lear $ \p a -> ((p, a) ^. l, go p a)
  where
    -- go :: p -> a -> Diff b -> (Diff a, Diff p)
    go p a db = splitDiff $ (\b -> swap $ (p, a) & l .~ b) <$> db

-- ((p, a) ^. l, \b' -> bimap (Endo . const) (Endo . const) $ (p, a) & l .~ b')

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
