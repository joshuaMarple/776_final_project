import Euterpea

-- x = line (map fn [(c,e), (d,f), (e,g), (f, a), (g,b), (a,c), (b, d)]) 
    -- where fn (n1, n2) = n1 4 qn :=: n2 4 qn

-- y = line (map fn [(c,e), (d,f), (e,g), (f, a), (g,b), (a,c), (b, d)]) 
--     where fn (n1, n2) = n1 2 hn :=: n2 2 hn

-- z = line [c 4 hn, d 4 hn, e 4 hn, f 4 hn, g 4 hn, a 4 hn]

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

samplePath = line $ [diminishedSeventh (cs 4 qn), minorTriad (d 4 qn), majorTriad (g 4 qn), minorTriad (e 4 qn)] ++
                    [minorTriad (a 4 qn), majorTriad (f 4 qn),  minorTriad (d 4 qn), majorTriad (g 4 qn), majorTriad (c 4 qn)]

comp = tempo (1/2) (l1 :+: (r :=: l))
main = writeMidi "test2.midi" samplePath
