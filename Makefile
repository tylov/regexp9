CC =		gcc -std=c99
CFLAGS =	-pedantic -Wall -O2
OBJS =		regexp9.o test.o
DFLAGS =
INCLUDES =
LIBS =

.SUFFIXES:.c .o

.c.o:
		$(CC) -c $(CFLAGS) $(INCLUDES) $(DFLAGS) $< -o $@

all:test

test:$(OBJS)
		$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

cleanlocal:
		rm -f *.o a.out *~ test

clean:cleanlocal
