TESTS=$(sort $(basename $(wildcard *.c)))
TARGETS=native

CC = gcc
CC_IA32 = gcc -m32
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

lightening-%.o: ../lightening.h ../lightening/*.c
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o $@ -c ../lightening/lightening.c

test-native-%: %.c lightening-native.o test.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o $@ lightening-native.o $<

test-ia32-%: CC = $(CC_IA32)
test-ia32-%: %.c lightening-ia32.o test.h
	$(CC) -m32 $(CFLAGS) $(CPPFLAGS) -I.. -o $@ lightening-ia32.o $<

clean:
	rm -f $(foreach TARGET,$(TARGETS),$(addprefix test-$(TARGET)-,$(TESTS)))
	rm -f $(foreach TARGET,$(TARGETS),lightening-$(TARGET).o)
