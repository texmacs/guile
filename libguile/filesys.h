#ifndef SCM_FILESYS_H
#define SCM_FILESYS_H

/* Copyright 1995,1997-2001,2006,2008-2011,2013,2018
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



#include <libguile/error.h>



SCM_API scm_t_bits scm_tc16_dir;

#define SCM_DIR_FLAG_OPEN (1L << 0)

#define SCM_DIRP(x) (SCM_HAS_TYP16 (x, scm_tc16_dir))
#define SCM_DIR_OPEN_P(x) (SCM_SMOB_FLAGS (x) & SCM_DIR_FLAG_OPEN)

#define SCM_VALIDATE_DIR(pos, port) \
  SCM_MAKE_VALIDATE_MSG (pos, port, DIRP, "directory port")



SCM_API SCM scm_chown (SCM object, SCM owner, SCM group);
SCM_API SCM scm_chmod (SCM object, SCM mode);
SCM_API SCM scm_umask (SCM mode);
SCM_API SCM scm_open_fdes (SCM path, SCM flags, SCM mode);
SCM_API SCM scm_open (SCM path, SCM flags, SCM mode);
SCM_API SCM scm_close (SCM fd_or_port);
SCM_API SCM scm_close_fdes (SCM fd);
SCM_API SCM scm_stat (SCM object, SCM exception_on_error);
SCM_API SCM scm_link (SCM oldpath, SCM newpath);
SCM_API SCM scm_rename (SCM oldname, SCM newname);
SCM_API SCM scm_delete_file (SCM str);
SCM_API SCM scm_mkdir (SCM path, SCM mode);
SCM_API SCM scm_rmdir (SCM path);
SCM_API SCM scm_directory_stream_p (SCM obj);
SCM_API SCM scm_opendir (SCM dirname);
SCM_API SCM scm_readdir (SCM port);
SCM_API SCM scm_rewinddir (SCM port);
SCM_API SCM scm_closedir (SCM port);
SCM_API SCM scm_chdir (SCM str);
SCM_API SCM scm_getcwd (void);
SCM_API SCM scm_select (SCM reads, SCM writes, SCM excepts, SCM secs, SCM msecs);
SCM_API SCM scm_fcntl (SCM object, SCM cmd, SCM value);
SCM_API SCM scm_fsync (SCM object);
SCM_API SCM scm_symlink (SCM oldpath, SCM newpath);
SCM_API SCM scm_readlink (SCM path);
SCM_API SCM scm_lstat (SCM str);
SCM_API SCM scm_copy_file (SCM oldfile, SCM newfile);
SCM_API SCM scm_dirname (SCM filename);
SCM_API SCM scm_basename (SCM filename, SCM suffix);
SCM_API SCM scm_canonicalize_path (SCM path);
SCM_API SCM scm_sendfile (SCM out, SCM in, SCM count, SCM offset);
SCM_INTERNAL SCM scm_i_relativize_path (SCM path, SCM in_path);

SCM_INTERNAL void scm_init_filesys (void);

#endif  /* SCM_FILESYS_H */
