SOURCE=main.hs dump.hs command.hs
MAIN=dvidump

parse : $(SOURCE)
	ghc $^ -Wall -o $(MAIN)


clean :
	rm -f *.ho *.hi *.o $(MAIN)
