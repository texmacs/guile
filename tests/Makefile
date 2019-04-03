TESTS=$(sort $(basename $(wildcard *.c)))

CC = gcc
CFLAGS = -Wall -O0 -g

all: $(addprefix test-,$(TESTS))

check: all
	@echo "Running unit tests..."
	@set -e; for test in $(TESTS); do \
	  echo "Testing: $$test"; \
	  ./test-$$test; \
	done
	@echo "Success."

lightening.o: ../lightening.h ../lightening/*.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o lightening.o -c ../lightening/lightening.c

test-%: %.c lightening.o test.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o $@ lightening.o $<

clean:
	rm -f $(addprefix test-,$(TESTS))
	rm -f lightening.o
