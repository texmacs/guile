TESTS=$(sort $(basename $(wildcard *.c)))
TARGETS=native

CC = gcc
CFLAGS = -Wall -O0 -g

all: $(foreach TARGET,$(TARGETS),$(addprefix test-$(TARGET)-,$(TESTS)))

check: $(addprefix test-$(TARGET),$(TARGETS))

test-%: $(addprefix test-%-,$(TESTS))
	@echo "Running unit tests..."
	@set -e; for test in $?; do \
	  echo "Testing: $$test"; \
	  ./$$test; \
	done
	@echo "Success."

.PHONY: test check

lightening.o: ../lightening.h ../lightening/*.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o lightening.o -c ../lightening/lightening.c

test-native-%: %.c lightening.o test.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o $@ lightening.o $<

clean:
	rm -f $(foreach TARGET,$(TARGETS),$(addprefix test-$(TARGET)-,$(TESTS)))
	rm -f lightening.o
