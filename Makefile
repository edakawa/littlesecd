CFLAGS=-O2 -Wall

.PHONY: clean test

littlesecd: littlesecd.c

clean:
	rm -f littlesecd *~

test: littlesecd
	@./test.ksh
