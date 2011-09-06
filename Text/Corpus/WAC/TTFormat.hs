-- | Parses WAC corpora from the TreeTagger format.
module Text.Corpus.WAC.TTFormat
    ( -- * Attoparsec parsers
      wordP
    , sentenceP
      -- * 'I.Iteratee' for the WAC corpus
    , corpusI
    ) where

import Data.ByteString (ByteString)
import Data.Word (Word8)

import Data.Attoparsec
import Control.Applicative ((*>),(<*))

import Text.Corpus.WAC.Common

-- | Generic per-line word parser
wordP :: Parser (Word ByteString)
wordP = do surface <- takeTill (==tab8)
           tag     <- skip (==tab8) *> takeTill (==tab8)
           lemma   <- skip (==tab8) *> takeTill (==nl8)
           skip (==nl8)
           return $ Word (surface , tag , lemma)

sentenceP :: Parser (Sentence ByteString)
sentenceP = return =<< choice [manyTill wordP (word8 nl8), many wordP]

c28 :: Char -> Word8
c28 = fromIntegral.fromEnum
{-# INLINE c28 #-}

nl8, tab8 :: Word8
nl8  = c28 $! '\n'
tab8 = c28 $! '\t'

corpusP :: Parser [Sentence ByteString]
corpusP = undefined

corpusI = undefined
