SOURCE=parse.hs dump.hs command.hs
MAIN=parse

parse : $(SOURCE)
	ghc $^ -o $(MAIN)


clean :
	rm -f *.ho *.hi *.o $(MAIN)
