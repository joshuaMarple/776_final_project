module Melody where

import Euterpea
import Chords
import qualified Data.Map as Map
import Data.Random
import Data.Random.Extras
import Data.Random.Source.DevRandom

melody :: (Octave -> Dur -> Music Pitch) -> String -> IO (Music Pitch)
melody root chord = do 
                x <- runRVar (choice [1..8]) DevRandom 
                let offNote = transpose (indexToNote Map.! x) (root 4 qn) 
                let mima = majorminor Map.! chord
                shufList <- runRVar (Data.Random.shuffle (mima (root 4 qn) ++ [offNote])) DevRandom
                return $ line shufList

majorminor = Map.fromList [("major", majorTriadList),
                           ("minor", minorTriadList)]
            
indexToNote = Map.fromList [(1, 0),
                   (2, 2),
                   (3, 4),
                   (4, 5),
                   (5, 7),
                   (6, 9),
                   (7, 11),
                   (8, 12)]
