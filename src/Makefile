INCLUDE_DIRECTORIES = -ifrontend:backend

all:
	$(MAKE) clean
	ghc --make Main.hs $(INCLUDE_DIRECTORIES) -o latc

clean:
	rm -f latc *.hi *.o *.log *.aux *.dvi
	rm -f frontend/*.hi frontend/*.o backend/*.hi backend/*.o Latte/*.hi Latte/*.o
