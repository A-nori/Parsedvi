TEX=tex
SOURCE=$(notdir $(shell pwd))
PACKAGES=~/texmf/tex/local/mymacros.sty
OPTIONS=-shell-escape
PCKDIR=

all : $(SOURCE).dvi $(SOURCE).pdf  result


$(SOURCE).dvi : $(SOURCE).tex $(PACKAGES)
	$(TEX) $(OPTIONS) $<

$(SOURCE).pdf : $(SOURCE).dvi
	dvipdfmx $<

result : 
	evince $(SOURCE).pdf &

send :
	for i in $(PACKAGES)
	do
		cp ${i} $(PCKDIR)
	done
