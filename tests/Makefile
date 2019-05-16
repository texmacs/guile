TESTS=$(sort $(basename $(wildcard *.c)))
TARGETS=native ia32 aarch64

# Suitable values of cross-compiler variables for Debian:
#
#   make test CC_IA32=i668-linux-gnu-gcc CC_AARCH64=aarch64-linux-gnu-gcc
#
# The relevant packages that you need to run this:
#
#   dpkg --add-architecture i386
#   dpkg --add-architecture arm64
#   apt-get update -qq
#   apt-get install -y \
#     libc6-dev:amd64 gcc make \
#     qemu binfmt-support qemu-user-static \
#     gcc-i686-linux-gnu libc6-dev-i386-cross libc6:i386 \
#     gcc-aarch64-linux-gnu libc6-dev-arm64-cross libc6:arm64
#
CC = gcc
CC_IA32=guix environment --pure -s i686-linux --ad-hoc gcc-toolchain glibc -- gcc
CC_AARCH64=guix environment --pure -s aarch64-linux --ad-hoc gcc-toolchain glibc -- gcc
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
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o $@ lightening-ia32.o $<

test-aarch64-%: CC = $(CC_AARCH64)
test-aarch64-%: %.c lightening-aarch64.o test.h
	$(CC) $(CFLAGS) $(CPPFLAGS) -I.. -o $@ lightening-aarch64.o $<

.PRECIOUS: $(foreach TARGET,$(TARGETS),$(addprefix test-$(TARGET)-,$(TESTS)))
.PRECIOUS: $(foreach TARGET,$(TARGETS),lightening-$(TARGET).o)

clean:
	rm -f $(foreach TARGET,$(TARGETS),$(addprefix test-$(TARGET)-,$(TESTS)))
	rm -f $(foreach TARGET,$(TARGETS),lightening-$(TARGET).o)
