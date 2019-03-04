SOURCE=parse.hs dump.hs command.hs

parse : $(SOURCE)
	ghc $^
