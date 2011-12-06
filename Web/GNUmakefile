WEB_DIR=www.omegahat.org:/home3/WebSites/Omega/Aspell


shipIndex: index.html extra
#	$(MAKE) -C ../inst/doc ship
	scp index.html $(WEB_DIR)/index.html

extra:
	scp ../INSTALL ../Changes.html ../FAQ.html $(WEB_DIR)

indexInstall indexOnly: index.html
	scp index.html $(WEB_DIR)/


shipDoc: ../inst/doc/aspell.html html
	scp -r html ../inst/doc/aspell.html $(WEB_DIR) 

html:
	if ! test -d Temp ; then mkdir Temp ; fi
	R CMD INSTALL -l Temp ..
	cp -r Temp/Aspell/html .
	rm -fr Temp

index.html: index.html.in configure
	./configure

configure: configure.in
	autoconf


%.html: %.xml
	$(MAKE) -C $(@D) $(@F)

