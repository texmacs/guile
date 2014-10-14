/*
 * Copyright (C) 2013  Free Software Foundation, Inc.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GNU lightning is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 */

#include <lightning.h>
#include <lightning/jit_private.h>
#include <stdio.h>

jit_int16_t	_szs[jit_code_last_code + 1];

int
main(int argc, char *argv[])
{
    FILE		*fp;
    jit_word_t		 offset;
    int			 code, size, max;

    if ((fp = fopen(JIT_SIZE_PATH, "r")) == NULL)
	exit(-1);
    while (fscanf(fp, "%d %d\n", &code, &size) == 2) {
	if (_szs[code] < size)
	    _szs[code] = size;
    }
    fclose(fp);

    max = 0;
    for (offset = 0; offset <= jit_code_last_code; offset++)
	if (max < _szs[offset])
	    max = _szs[offset];

    if ((fp = fopen(JIT_SIZE_PATH, "w")) == NULL)
	exit(-1);


    fprintf(fp, "#if __WORDSIZE == %d\n", __WORDSIZE);
#if defined(__arm__)
#  if defined(__ARM_PCS_VFP)
    fprintf(fp, "#if defined(__ARM_PCS_VFP)\n");
#  else
    fprintf(fp, "#if !defined(__ARM_PCS_VFP)\n");
#  endif
#elif defined(__mips__)
#  if __WORDSIZE == 32
#    if NEW_ABI
    fprintf(fp, "#if NEW_ABI\n");
#    else
    fprintf(fp, "#if !NEW_ABI\n");
#    endif
#  endif
#elif defined(__ppc__)
    fprintf(fp, "#if defined(__ppc__)\n");
#elif defined(__powerpc__)
    fprintf(fp, "#if defined(__powerpc__)\n");
#endif
    fprintf(fp, "#define JIT_INSTR_MAX %d\n", max);
    for (offset = 0; offset <= jit_code_last_code; offset++)
	fprintf(fp, "    %d,\n", _szs[offset]);
#if defined(__arm__)
    fprintf(fp, "#endif /* __ARM_PCS_VFP */\n");
#elif defined(__mips__)
#  if __WORDSIZE == 32
    fprintf(fp, "#endif /* NEW_ABI */\n");
#  endif
#elif defined(__ppc__)
    fprintf(fp, "#endif /* __ppc__ */\n");
#elif defined(__powerpc__)
    fprintf(fp, "#endif /* __powerpc__ */\n");
#endif
    fprintf(fp, "#endif /* __WORDSIZE */\n");
    fclose(fp);

    return (0);
}
