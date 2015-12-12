all:
	ghc -threaded ./Main.hs && ./Main && timidity ./test.midi

clean:
	rm ./Main || true
	rm *.midi || true
	rm *.hi || true
	rm *.o || true
