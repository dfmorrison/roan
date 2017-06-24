SOURCES=package.lisp util.lisp roan.lisp pattern.lisp method.lisp \
	tests.lisp util-tests.lisp roan-tests.lisp pattern-tests.lisp method-tests.lisp \
	extract-documentation.lisp

all: clean TAGS documentation

documentation: doc/roan.pdf doc/roan.info doc/roan.html doc/roan/index.html

pdf: doc/roan.pdf

html: doc/roan.html doc/roan/index.html

info: doc/roan.info

doc/roan.pdf: doc/roan.texi doc/inc/roan-version.texi
	cd doc; makeinfo --pdf roan.texi

doc/roan.info: doc/roan.texi doc/inc/roan-version.texi
	cd doc; makeinfo roan.texi

doc/roan.html: doc/roan.texi doc/inc/roan-version.texi
	cd doc; makeinfo --html --css-include=roan.css --no-split roan.texi

doc/roan/index.html: doc/roan.texi doc/inc/roan-version.texi
	cd doc; makeinfo --html --css-include=roan.css --split=chapter roan.texi

doc/inc/roan-version.texi: $(SOURCES) roan.asd extract-documentation.lisp
	ccl -Q \
	-e '(ql:quickload :roan/doc)' \
	-e '(roan/doc:extract-documentation :roan)' \
	-e '(quit)'

update-database:
	ccl -Q \
	-e '(ql:quickload :roan/db)' \
	-e '(roan::update-method-database :create-if-does-not-exist t)' \
	-e '(quit)'
tidy:
	-rm -rf doc/inc doc/roan.aux doc/roan.fn doc/roan.fns doc/roan.log doc/roan.toc \
	        doc/roan.vr doc/roan.vrs doc/roan.tp doc/roan.tps doc/roan.cp doc/roan.cps

clean: tidy
	-rm -rf doc/roan doc/roan.pdf doc/roan.info doc/roan.html

TAGS: $(SOURCES)
	etags $(SOURCES)

touch:
	touch doc/roan.texi
