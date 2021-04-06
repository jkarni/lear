{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lear.Internal.Orphans where

import System.Random

instance Random () where
  randomR _ g = ((), g)
  random g = ((), g)

instance (Random a, Random b) => Random (a, b) where
  randomR ((lowL, lowR), (hiL, hiR)) g =
    let (l, g') = randomR (lowL, hiL) g
        (r, g'') = randomR (lowR, hiR) g'
     in ((l, r), g'')
  random g =
    let (l, g') = random g
        (r, g'') = random g'
     in ((l, r), g'')
