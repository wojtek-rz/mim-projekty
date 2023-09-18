CFLAGS = -Wall -Wextra -Wno-implicit-fallthrough -std=c17 -O2
CC=gcc
cfiles = $(wildcard *.c)
objects = $(cfiles:.c=.o)

main: $(objects)
	$(CC) $(CFLAGS) -o main.exe $(objects)

array.o: array.c array.h errors.h types.h bitvectors.h
bfs.o: bfs.c bfs.h array.h bitvectors.h types.h queue.h
bitvectors.o: bitvectors.c bitvectors.h errors.h
errors.o: errors.c errors.h
input.o: input.c input.h array.h bitvectors.h types.h errors.h bfs.h
main.o: main.c array.h input.h bitvectors.h types.h bfs.h
queue.o: queue.c queue.h errors.h types.h array.h bitvectors.h

clean:
	-rm $(objects)