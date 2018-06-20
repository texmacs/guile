#ifndef SCM_SYSCALLS_H
#define SCM_SYSCALLS_H

/* Copyright 1995-1996,2000-2002,2006,2008-2011,2013-2014,2018
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */



/* ASYNC_TICK after finding EINTR in order to handle pending signals, if
   any. See comment in scm_syserror. */
#define SCM_SYSCALL(line)			\
  do						\
    {						\
      errno = 0;				\
      line;					\
      if (errno == EINTR)			\
	{					\
          scm_async_tick ();                    \
	  errno = EINTR;			\
	}					\
    }						\
  while (errno == EINTR)




#if defined GUILE_USE_64_CALLS && GUILE_USE_64_CALLS && defined(HAVE_STAT64)
#define CHOOSE_LARGEFILE(foo,foo64)     foo64
#else
#define CHOOSE_LARGEFILE(foo,foo64)     foo
#endif

/* These names are a bit long, but they make it clear what they represent. */
#if SCM_HAVE_STRUCT_DIRENT64 == 1
# define dirent_or_dirent64             CHOOSE_LARGEFILE(dirent,dirent64)
#else
# define dirent_or_dirent64             dirent
#endif
#define fstat_or_fstat64                CHOOSE_LARGEFILE(fstat,fstat64)
#define ftruncate_or_ftruncate64        CHOOSE_LARGEFILE(ftruncate,ftruncate64)
#define lseek_or_lseek64                CHOOSE_LARGEFILE(lseek,lseek64)
#define lstat_or_lstat64                CHOOSE_LARGEFILE(lstat,lstat64)
#define off_t_or_off64_t                CHOOSE_LARGEFILE(off_t,off64_t)
#define open_or_open64                  CHOOSE_LARGEFILE(open,open64)
#define readdir_or_readdir64            CHOOSE_LARGEFILE(readdir,readdir64)
#if SCM_HAVE_READDIR64_R == 1
# define readdir_r_or_readdir64_r       CHOOSE_LARGEFILE(readdir_r,readdir64_r)
#else
# define readdir_r_or_readdir64_r       readdir_r
#endif
#define stat_or_stat64                  CHOOSE_LARGEFILE(stat,stat64)
#define truncate_or_truncate64          CHOOSE_LARGEFILE(truncate,truncate64)
#define scm_from_off_t_or_off64_t       CHOOSE_LARGEFILE(scm_from_off_t,scm_from_int64)
#define scm_from_ino_t_or_ino64_t       CHOOSE_LARGEFILE(scm_from_ulong,scm_from_uint64)
#define scm_from_blkcnt_t_or_blkcnt64_t CHOOSE_LARGEFILE(scm_from_ulong,scm_from_uint64)
#define scm_to_off_t_or_off64_t         CHOOSE_LARGEFILE(scm_to_off_t,scm_to_int64)

#if SIZEOF_OFF_T == 4
#  define scm_to_off_t    scm_to_int32
#  define scm_from_off_t  scm_from_int32
#elif SIZEOF_OFF_T == 8
#  define scm_to_off_t    scm_to_int64
#  define scm_from_off_t  scm_from_int64
#else
#  error sizeof(off_t) is not 4 or 8.
#endif
#define scm_to_off64_t    scm_to_int64
#define scm_from_off64_t  scm_from_int64


#endif  /* SCM_SYSCALLS_H */
