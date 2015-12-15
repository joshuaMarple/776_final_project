import Euterpea
import qualified Data.Map as Map
import Chords
import Melody
import System.Random
import Data.Random
import Data.Random.Extras 
import Data.Random.Source.DevRandom

type ChordProg = Map.Map String (Music Pitch)

c_progList = [("Dm", ["G",  "Em", "C"]),
              ("G",  ["Em", "Am", "C"]),
              ("Em", ["Am", "F"]), 
              ("Am", ["F",  "Dm"]),
              ("F",  ["Dm", "G", "C"]), 
              ("C",  ["Dm", "G", "Em", "Am", "F"])]

c_mima = Map.fromList [("Dm", (d, "minor")),
              ("G", (g, "major")),
              ("Em", (e, "minor")), 
              ("Am", (a, "minor")),
              ("F", (f, "major")), 
              ("C", (c, "major"))]
              

note_len = en
l_note_len = dqn
-- c_chordInterpList = [("Dm", minorComp (d 4 note_len) (d 3 l_note_len)),
--                      ("G",  majorComp (g 4 note_len) (g 3 l_note_len)),
--                      ("Em", minorComp (e 4 note_len) (e 3 l_note_len)),
--                      ("Am", minorComp (a 4 note_len) (a 3 l_note_len)),
--                      ("F",  majorComp (f 4 note_len) (f 3 l_note_len)),
--                      ("C",  majorComp (c 4 note_len) (c 3 l_note_len))]

c_chordInterpList = [("Dm", minorTriad (d 3 l_note_len)),
                     ("G",  majorTriad (g 3 l_note_len)),
                     ("Em", minorTriad (e 3 l_note_len)),
                     ("Am", minorTriad (a 3 l_note_len)),
                     ("F",  majorTriad (f 3 l_note_len)),
                     ("C",  majorTriad (c 3 l_note_len))]

chordInterp = Map.fromList c_chordInterpList

c_progMap = Map.fromList c_progList

c_progKeys = Map.keys c_progMap
numKeys = length c_progKeys

genStart :: IO ()
genStart = do
    x <- runRVar (choice c_progKeys) DevRandom
    genSequence x (line []) 5

genSequence :: String -> Music Pitch -> Int -> IO ()
genSequence x y cur_len = do 
    next <- runRVar (choice (c_progMap Map.! x)) DevRandom
    let val = c_mima Map.! x
    mel  <- melody (fst val) (snd val)

    if ((x == "C") && (cur_len <= 0))
        then writeMidi "test.midi" (y :+: (majorTriad (c 3 l_note_len) :=: majorTriad (c 4 l_note_len)))
        else genSequence next (y :+: (chordInterp Map.! x :=: mel)) (cur_len - 1)
              

la = line (map fn [(d, 3), (e, 3), (f, 3), (a, 3), (c, 4), (e, 4)]) where fn (n1, oc) = n1 oc sn
lb = line (map fn [(g, 4), (e, 4), (c, 4), (a, 3), (f, 3), (e, 3)]) where fn (n1, oc) = n1 oc sn
lc = line (map fn [(d, 3), (e, 3), (f, 3), (a, 3), (c, 4), (e, 4)]) where fn (n1, oc) = n1 oc sn

l1 = la :+: la :+: la :+: lb
l2 = la :+: la :+: la :+: la :+: lc :+: lc :+: lc :+: lc 
l = l2 :+: l2
 
r1 = line [a 4 qn, f 5 en, e 5 qn, c 5 en, b 4 qn, a 4 sn, g 4 sn, b 4 qn, rest en, rest qn, rest en] 
r2 = line [d 6 sn, a 6 sn, e 6 en]
r3 = line [f 6 sn, c 7 sn, g 6 sn, f 6 sn, e 6 qn, rest en, rest qn, rest en, a 6 qn, rest hn]
r4 = line [a 4 qn, f 5 en, e 5 qn, c 5 en, d 5 qn, e 5 sn, f 5 sn, g 5 qn, rest en, rest hn] 
r = r1 :+: r2 :+: r2 :+: line [rest qn, rest qn, rest en] :+: r4 :+: r3

x = line (map ($ c 4 wn) [majorTriad, minorTriad, augmentedTriad, diminishedTriad, augmentedSeventh, augmentedMajorSeventh, diminishedSeventh])

samplePath = line $ [diminishedSeventh (cs 4 qn), minorTriad (d 4 qn), majorTriad (g 4 qn), minorTriad (e 4 qn)] ++
                    [minorTriad (a 4 qn), majorTriad (f 4 qn),  minorTriad (d 4 qn), majorTriad (g 4 qn), majorTriad (c 4 qn)]

comp = tempo (1/2) (l1 :+: (r :=: l))
main =  genStart
