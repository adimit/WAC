-- | Representation of the WAC data structure within Haskell.
--   'Word' captures all three fields present in the WAC data.
module Text.Corpus.WAC
    ( -- * Types
      Word(..)
    , Sentence(..)
      -- * Utility functions
    , posTag
    , surface
    , lemma
    ) where

import Text.Corpus.WAC.Common
