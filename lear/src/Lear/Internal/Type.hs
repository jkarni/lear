module Lear.Internal.Type where

import Control.Applicative
import qualified Control.Category as C
import Data.VectorSpace
  ( AdditiveGroup (..),
    VectorSpace (..),
  )
import Debug.Trace
import GHC.Generics (Generic)

newtype Lear p a b
  = Lear
      -- We return a p -> p rather than a p so that updates can be composed.
      -- This is more general than using a product, and also nicer to use since
      -- you can keep classy lenses over p.
      -- That said, I'm not sure updates that are not independent make sense
      -- (prob: that don't commute).
      { getLear :: p -> a -> (b, b -> (p -> p, a))
      }
  deriving (Generic)

liftOp ::
  (Num a, Num p, Num b, Show a, Show p, Show b) =>
  (forall a. Num a => a -> a -> a) ->
  Lear p a b ->
  Lear p a b ->
  Lear p a b
liftOp (?) (Lear x) (Lear y) = Lear $ \p a ->
  let (bx, linx) = x (trace ("p=" ++ show p) p) (trace ("a=" ++ show a) a)
      (by, liny) = y p (trace ("a=" ++ show a) a)
   in ( (trace ("bx=" ++ show bx) bx) ? (trace ("by=" ++ show by) by),
        \b' ->
          let (fpx, ax) = linx b'
              (fpy, ay) = liny (trace ("b'=" ++ show b') b')
           in -- Not sure about ax ? ay...
              (fpx . fpy, trace ("ax=" ++ show ax) ax - trace ("ay=" ++ show ay) ay)
      )

instance (Num a, Num p, Num b, Show p, Show b, Show a) => Num (Lear p a b) where
  fromInteger x = Lear $ \p a -> (fromInteger x, const (const p, a))
  (+) = liftOp (+)
  (-) = liftOp (-)
  (*) = liftOp (*)

instance C.Category (Lear p) where
  id = Lear $ \_ a -> (a, const (id, a))

  Lear g . Lear f = Lear $ \p a ->
    let (b, f') = f p a
        (c, g') = g p b
     in ( c,
          \c' ->
            let (pg, b') = g' c'
                (pf, a') = f' b'
             in (pf . pg, a')
        )

instance
  (AdditiveGroup p, AdditiveGroup a, AdditiveGroup b) =>
  AdditiveGroup (Lear p a b)

instance
  ( VectorSpace p,
    VectorSpace a,
    VectorSpace b,
    Scalar p ~ Scalar a,
    Scalar p ~ Scalar b
  ) =>
  VectorSpace (Lear p a b)
  where
  type Scalar (Lear p a b) = Scalar p

  s *^ Lear f = Lear $ \p a ->
    let (b, lin) = f p a
     in ( s *^ b,
          \b' ->
            let (updP, a') = lin b'
             in (\p' -> s *^ updP p', s *^ a')
        )
