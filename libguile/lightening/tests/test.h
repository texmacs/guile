#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>

#include <lightening.h>

#define ASSERT(x) \
  do {                                                                  \
    if (!(x)) {                                                         \
      fprintf(stderr, "%s:%d: assertion failed: " #x "\n",              \
              __FILE__, __LINE__);                                      \
      abort();                                                          \
    }                                                                   \
  } while (0)

static inline int
main_helper (int argc, char *argv[],
             void (*run_test)(jit_state_t*, uint8_t*, size_t))
{
  ASSERT(init_jit());
  jit_state_t *j = jit_new_state (NULL, NULL);
  ASSERT(j);

  const size_t arena_size = 4096;
  char *arena_base = mmap (NULL, arena_size,
                           PROT_EXEC | PROT_READ | PROT_WRITE,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

  if (arena_base == MAP_FAILED)
    {
      perror ("allocating JIT code buffer failed");
      return 1;
    }

  run_test(j, (uint8_t*)arena_base, arena_size);
  
  jit_destroy_state(j);

  munmap(arena_base, arena_size);

  return 0;
}
