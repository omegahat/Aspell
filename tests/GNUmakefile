ASPELL_LIBS=-laspell

CFLAGS=-g -Wall
CC=gcc

config: config.o
	$(CC) -o $@ $^ $(ASPELL_LIBS)

list: list.o
	$(CC) -o $@ $^ $(ASPELL_LIBS)

modules: modules.o
	$(CC) -o $@ $^ $(ASPELL_LIBS)

clean:
	- rm *.o
	- rm modules list config

