ZIP_SOURCE := solution
ZIP_TARGET := bs429594-Latte.tar.gz

all:
	$(MAKE) clean
	$(MAKE) -C src all
	cp src/latc .

clean:
	rm -rf latc ${ZIP_SOURCE} ${ZIP_TARGET}
	$(MAKE) -C src clean

zip:
	$(MAKE) clean
	mkdir ${ZIP_SOURCE}
	cp README.md ${ZIP_SOURCE}/
	cp Makefile ${ZIP_SOURCE}/
	cp -r src ${ZIP_SOURCE}/
	tar -czvf ${ZIP_TARGET} -C ${ZIP_SOURCE} .
