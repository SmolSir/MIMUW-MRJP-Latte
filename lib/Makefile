all:
	$(MAKE) clean
	gcc -m32 -D_GNU_SOURCE -z execstack -c runtime.c -o runtime.o

clean:
	rm -f runtime.o
