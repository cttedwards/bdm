# Take a look at: https://gist.github.com/halpo/1374344

PKG_VERSION=$(shell grep -i ^version ./DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package ./DESCRIPTION | cut -d : -d \  -f 2)

R_FILES := $(wildcard ./R/*.R)

ifdef ComSpec
	RM=del /F /Q
then
	RM=rm -rf
endif

linux: $(R_FILES)
	R --vanilla -e 'source("version_update.R"); roxygen2::roxygenize(".")'
	chmod 777 DESCRIPTION
	R CMD INSTALL --build .

windows: $(R_FILES)
    Rscript version_update.R
	Rcmd INSTALL --build .
	
vignettes:
	R --vanilla -e 'devtools::build_vignettes()'
	
#vignettes: $(VIGNETTE_FILES)
#	R --vanilla -e 'knitr::knit(\"vignettes\bdm-examples.Rmd\")'
#	#pandoc -V geometry:margin=1in bdm-examples.md -o bdm-examples.pdf --bibliography=bdm.bib 
#	#pandoc -V geometry:margin=1in bdm-examples.md -o bdm-examples.html --bibliography=bdm.bib

clean:
	$(RM) $(PKG_NAME)_*.tar.gz
	$(RM) $(PKG_NAME)_*.zip

###################################################	
## Makefile to use knitr for package vignettes
#
## put all PDF targets here, separated by spaces
#PDFS= knitr-intro.pdf knitr-refcard.pdf knitr-markdown.html
#
#all: $(PDFS) 
#
#clean:
#	rm -rf *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.md figure/
#
#%.pdf: %.Rnw
#	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2pdf('$*.Rnw') else tools::texi2pdf('$*.tex')"
#
#%.html: %.Rmd
#	$(R_HOME)/bin/Rscript -e "if (getRversion() < '3.0.0') knitr::knit2html('$*.Rmd')"
#