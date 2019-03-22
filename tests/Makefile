TESTS = addr

CC = gcc
CFLAGS = -Wall -O0 -g

all: $(addprefix test-,$(TESTS))

jit.o: ../jit.h ../jit/*.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -flto -I.. -o jit.o -c ../jit/jit.c

test-%: test-%.c jit.o test.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -flto -I.. -o $@ jit.o $<

clean:
	rm -f $(addprefix test-,$(TESTS))
	rm -f jit.o
