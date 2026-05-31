SOURCES=package.lisp util.lisp roan.lisp pattern.lisp method.lisp \
	tests.lisp util-tests.lisp roan-tests.lisp pattern-tests.lisp method-tests.lisp \
	extract-documentation.lisp

all: clean TAGS documentation website

documentation: doc/roan-manual.pdf doc/index.html

pdf: doc/roan-manual.pdf

html: doc/index.html

doc/roan-manual.pdf: doc/roan.texi doc/inc/roan-version.texi
	cd doc; makeinfo --pdf roan.texi -o roan-manual.pdf

doc/index.html: doc/roan.texi doc/inc/roan-version.texi
	cd doc; makeinfo --html --css-include=roan.css --no-split roan.texi -o index.html

doc/inc/roan-version.texi: $(SOURCES) roan.asd extract-documentation.lisp
	sbcl --noinform --non-interactive \
	--eval '(ql:quickload :roan/doc)' \
	--eval '(roan/doc:extract-documentation :roan)' \

website: doc/index.html doc/roan-manual.pdf
	cd doc; cp index.html roan-manual.pdf ../../roan-doc/

tidy:
	-rm -rf doc/inc doc/roan.aux doc/roan.fn doc/roan.fns doc/roan.log doc/roan.toc \
	        doc/roan.vr doc/roan.vrs doc/roan.tp doc/roan.tps doc/roan.cp doc/roan.cps

clean: tidy
	-rm -rf doc/roan doc/roan-manual.pdf doc/index.html

TAGS: $(SOURCES)
	etags $(SOURCES)

touch:
	touch doc/roan.texi
