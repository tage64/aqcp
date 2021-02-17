-- Data types for audio samples and sample lists in a raw stream of audio.
module Samples (Sample, SampleList) where

import           Data.Int
import           Data.Vector.Storable

{- A single sample in a raw audio stream.
 - A 16 bit integer
 -}
type Sample = Int16

{- A list of samples.
 - Represented by a vector of samples.
 -}
type SampleList = Vector Sample
