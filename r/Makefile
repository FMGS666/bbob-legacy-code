R	:= R --vanilla
RSCRIPT	:= Rscript --vanilla
DELETE	:= rm -fR

.SILENT:
.PHONEY: clean test check build install package data usage help documentation putdocs

usage:
	echo "Available targets:"
	echo ""
	echo " install       - install the package"
	echo " check         - run R CMD check on the package"
	echo " package       - build source package"
	echo " help          - shows all available targets"
	echo " test          - run test suite"
	echo " documentation - build documentation"

help: usage
	echo " clean         - clean up package cruft"
	echo " pkg           - roxygenize skel/ into pkg/"
	echo " install-deps  - Install R dependencies."

install: clean pkg
	echo "Installing package ..."
	${R} CMD INSTALL pkg > install.log 2>&1

check: clean pkg
	echo "Running R CMD check ..."
	${R} CMD check pkg && ${DELETE} pkg.Rcheck

clean:
	echo "Cleaning up ..."
	${DELETE} skel/src/*.o skel/src/*.so skel.Rcheck
	${DELETE} pkg
	${DELETE} doc/index.html doc/README.pdf
	${DELETE} .RData .Rhistory build.log install.log roxygen.log package.log

documentation: doc/index.html doc/README.pdf

putdocs: documentation
	echo "ONLY FOR OLAF!"
	scp doc/index.html doc/bbob.css doc/README.pdf \
	  teth.0x200.net:/srv/http/bbob2012.p-value.net/public/

doc/index.html: doc/README.txt
	echo "Building $@"
	pandoc -f markdown -t html5 -c bbob.css \
	  --highlight-style=haddock \
	  -s $< -o $@ 

doc/README.pdf: doc/README.txt
	echo "Building $@"
	pandoc -f markdown -t latex -c bbob.css \
	  --highlight-style=haddock \
	  -s $< -o doc/README.tex
## Need to run pdflatex twice: 
	(cd doc; pdflatex README.tex > /dev/null)
	(cd doc; pdflatex README.tex > /dev/null)
	rm -f doc/README.tex doc/README.log doc/README.aux \
	  doc/README.toc doc/README.out

package: pkg
	echo "Building package file ..."
	${R} CMD build pkg/ > package.log 2>&1

pkg: clean
	cp -R skel pkg
	echo "Roxygenizing package ..."
	${RSCRIPT} ./tools/roxygenize > roxygen.log 2>&1
	echo "Setting 'Version' in DESCRIPTION ..."
	${RSCRIPT} ./tools/set-version 1 0
	rm -fR pkg/inst/

test: install
	${RSCRIPT} ./tools/run-tests

install-deps:
	echo "bbobr requires R 2.15.0 or newer. Make sure to use an up to date R!"
	Rscript -e 'install.packages(c("stringr", "BBmisc", "roxygen2"))'

