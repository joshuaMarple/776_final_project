all:
	ghc -threaded ./project.hs && ./project && timidity ./test2.midi

clean:
	rm ./project || true
	rm *.midi || true
	rm *.hi || true
	rm *.o || true
