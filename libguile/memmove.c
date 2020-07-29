/* Wrapper to implement ANSI C's memmove using BSD's bcopy. */
/* This function is in the public domain.  --Per Bothner. */


#include <sys/types.h>

#ifdef __STDC__
#define PTR void *
#define CPTR const void *
PTR memmove (PTR, CPTR, size_t);
#else
#define PTR char *
#define CPTR char *
PTR memmove ();
#endif

PTR
memmove (PTR s1, CPTR s2, size_t n)
{
//  bcopy (s2, s1, n);
  char* _s1 = (char*)s1;
  char* _s2 = (char*)s2;
  size_t i;
  for(i=0;i<n;i++) {
      *_s1 = *_s2;
      _s1++;
      _s2++;
  }
  return s1;
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
