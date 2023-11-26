INCLUDE_DIRECTORIES = -ifrontend

all:
	ghc --make Main.hs $(INCLUDE_DIRECTORIES) -o latc

clean:
	rm -f latc *.hi *.o *.log *.aux *.dvi
	rm -f frontend/*.hi frontend/*.o Latte/*.hi Latte/*.o
