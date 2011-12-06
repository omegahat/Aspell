VERSION=$(shell perl -e 'while(<>) { if(m/Version: (.*)/) {print $$1,"\n";}}' DESCRIPTION)

DOCS=inst/doc/aspell.html

fastInstall: copy
	if test -r configure ; then rm configure ; fi 
	R CMD INSTALL .


build: configure copy $(DOCS)
	- rm -f src/*.o src/*.so
	- $(MAKE) -C tests clean
	cd .. ; R CMD build Aspell 
	mv ../Aspell_${VERSION}.tar.gz .


check:  copy
	R CMD check .

install: configure copy
	 R CMD INSTALL .

configure: configure.in
	   autoconf


copy:   TODO.html INSTALL
	if ! test -d inst ; then mkdir inst ; fi
	cp TODO.html INSTALL inst/



%.html: %.xml
	$(MAKE) -C $(@D) $(@F)


#############################################################################


aspell.c.tu:

aspell.c: 
	echo "#include <aspell.h>" > $@
	echo "" >> $@

aspell.c.tu:  aspell.c GNUmakefile
	gcc-3.3 -c -fdump-translation-unit $< > $@
