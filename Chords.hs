module Chords where

import Euterpea
majorTriad :: Music a -> Music a
majorTriad x = chord [x, transpose 4 x, transpose 7 x]

minorTriad :: Music a -> Music a
minorTriad x = chord [x, transpose 3 x, transpose 7 x]

augmentedTriad:: Music a -> Music a
augmentedTriad x = chord [x, transpose 3 x, transpose 8 x]

diminishedTriad :: Music a -> Music a
diminishedTriad x = x :=: chord [transpose 3 x, transpose 6 x]

augmentedSeventh :: Music a -> Music a
augmentedSeventh x = chord [x, transpose 4 x, transpose 8 x, transpose 10 x]

augmentedMajorSeventh :: Music a -> Music a
augmentedMajorSeventh x = chord [x, transpose 4 x, transpose 8 x, transpose 11 x]

diminishedSeventh :: Music a -> Music a
diminishedSeventh x = chord [x, transpose 3 x, transpose 6 x, transpose 9 x]

