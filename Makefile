ZIP_SOURCE := solution
ZIP_TARGET := bs429594-Latte.tar.gz

all:
	$(MAKE) clean
	$(MAKE) -C src all
	cp src/latc_x86 .

clean:
	rm -rf latc_x86 ${ZIP_SOURCE} ${ZIP_TARGET}
	$(MAKE) -C src clean

zip:
	$(MAKE) clean
	mkdir ${ZIP_SOURCE}
	mkdir ${ZIP_SOURCE}/lib
	cp README.md ${ZIP_SOURCE}/
	cp Makefile ${ZIP_SOURCE}/
	cp -r src ${ZIP_SOURCE}/
	cp lib/runtime.o ${ZIP_SOURCE}/lib/
	tar -czvf ${ZIP_TARGET} -C ${ZIP_SOURCE} .
