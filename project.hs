import Euterpea

x = line (map fn [(c,e), (d,f), (e,g), (f, a), (g,b), (a,c), (b, d)]) 
    where fn (n1, n2) = n1 4 qn :=: n2 4 qn

main = do
     writeMidi "test2.midi" x
