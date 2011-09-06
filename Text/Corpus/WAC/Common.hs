{-# LANGUAGE DeriveFunctor #-}
module Text.Corpus.WAC.Common
    ( -- Data Types
      Word(..)
    , Sentence(..)
    -- * Deconstructors for 'Word'
    , posTag
    , surface
    , lemma
    -- * Utility functions
    , utf8bsToCaseFold ) where

import Data.Text (toCaseFold,Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (ByteString)

-- | Word type; essentially just a wrapper around a 3-tuple
--   Don't forget the functor instance. It's really handy to just 'fmap'
--   over the enclosed type.
newtype Word a = Word (a,a,a) deriving (Show, Ord, Eq, Functor)
type Sentence a = [Word a] -- ^ Sentence type: a list of words.

posTag, surface, lemma :: Word a -> a
surface (Word (a,_,_)) = a -- ^ Extract the surface form from a 'Word'
posTag  (Word (_,a,_)) = a -- ^ Extract the posTag from a 'Word'
lemma   (Word (_,_,a)) = a -- ^ Extract the lemma from a 'Word'

-- | Utility function. Uses 'decodeUtf8' and 'toCaseFold' on the
--   'surface' and 'lemma' fields of a 'Word', but not on the
--   'posTag' field.
utf8bsToCaseFold :: Word ByteString -> Word Text
utf8bsToCaseFold (Word (s,p,l)) = Word (fold s, decodeUtf8 p, fold l)
    where fold = toCaseFold.decodeUtf8
