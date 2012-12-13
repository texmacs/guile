/*
 * Copyright (C) 2012  Free Software Foundation, Inc.
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 */

#if defined(__linux__)
#  include <sys/cachectl.h>
#endif

#define rc(value)			jit_class_##value
#define rn(reg)				(jit_regno(_rvs[jit_regno(reg)].spec))

/* initial, mipsel 32 bits code only */

/*
 * Prototypes
 */
#  define patch(instr, node)		_patch(_jit, instr, node)
static void _patch(jit_state_t*,jit_word_t,jit_node_t*);

#define PROTO				1
#  include "jit_mips-cpu.c"
#  include "jit_mips-fpu.c"
#undef PROTO

/*
 * Initialization
 */
jit_register_t		_rvs[] = {
    { rc(gpr) | 0x01,			"at" },
    { rc(gpr) | 0x02,			"v0" },
    { rc(gpr) | 0x03,			"v1" },
    { rc(gpr) | 0x08,			"t0" },
    { rc(gpr) | 0x09,			"t1" },
    { rc(gpr) | 0x0a,			"t2" },
    { rc(gpr) | 0x0b,			"t3" },
    { rc(gpr) | 0x0c,			"t4" },
    { rc(gpr) | 0x0d,			"t5" },
    { rc(gpr) | 0x0e,			"t6" },
    { rc(gpr) | 0x0f,			"t7" },
    { rc(gpr) | 0x18,			"t8" },
    { rc(gpr) | 0x19,			"t9" },
    { rc(sav) | rc(gpr) | 0x10,		"s0" },
    { rc(sav) | rc(gpr) | 0x11,		"s1" },
    { rc(sav) | rc(gpr) | 0x12,		"s2" },
    { rc(sav) | rc(gpr) | 0x13,		"s3" },
    { rc(sav) | rc(gpr) | 0x14,		"s4" },
    { rc(sav) | rc(gpr) | 0x15,		"s5" },
    { rc(sav) | rc(gpr) | 0x16,		"s6" },
    { rc(sav) | rc(gpr) | 0x17,		"s7" },
    { 0x00,				"zero" },
    { 0x1a,				"k0" },
    { 0x1b,				"k1" },
    { rc(sav) | 0x1f,			"ra" },
    { rc(sav) | 0x1c,			"gp" },
    { rc(sav) | 0x1d,			"sp" },
    { rc(sav) | 0x1e,			"fp" },
    { rc(arg) | rc(gpr) | 0x07,		"a3" },
    { rc(arg) | rc(gpr) | 0x06,		"a2" },
    { rc(arg) | rc(gpr) | 0x05,		"a1" },
    { rc(arg) | rc(gpr) | 0x04,		"a0" },
    { rc(fpr) | 0x00,			"$f0" },
    { rc(fpr) | 0x02,			"$f2" },
    { rc(fpr) | 0x04,			"$f4" },
    { rc(fpr) | 0x06,			"$f6" },
    { rc(fpr) | 0x08,			"$f8" },
    { rc(fpr) | 0x0a,			"$f10" },
    { rc(sav) | rc(fpr) | 0x10,		"$f16" },
    { rc(sav) | rc(fpr) | 0x12,		"$f18" },
    { rc(sav) | rc(fpr) | 0x14,		"$f20" },
    { rc(sav) | rc(fpr) | 0x16,		"$f22" },
    { rc(sav) | rc(fpr) | 0x18,		"$f24" },
    { rc(sav) | rc(fpr) | 0x1a,		"$f26" },
    { rc(sav) | rc(fpr) | 0x1c,		"$f28" },
    { rc(sav) | rc(fpr) | 0x1e,		"$f30" },
    { rc(arg) | rc(fpr) | 0x0e,		"$f14" },
    { rc(arg) | rc(fpr) | 0x0c,		"$f12" },
    { _NOREG,				"<none>" },
};

/* Could also:
 *	o reserve a register for carry	(overkill)
 *	o use MTLO/MFLO			(performance hit)
 * So, keep a register allocated after setting carry, and implicitly
 * deallocate it if it can no longer be tracked
 */
static jit_int32_t	jit_carry;

/*
 * Implementation
 */
void
jit_get_cpu(void)
{
}

void
_jit_init(jit_state_t *_jit)
{
    _jit->reglen = jit_size(_rvs) - 1;
    jit_carry = _NOREG;
}

void
_jit_prolog(jit_state_t *_jit)
{
    jit_int32_t		 offset;

    if (_jit->function)
	jit_epilog();
    assert(jit_regset_cmp_ui(_jit->regarg, 0) == 0);
    jit_regset_set_ui(_jit->regsav, 0);
    offset = _jit->functions.offset;
    if (offset >= _jit->functions.length) {
	_jit->functions.ptr = realloc(_jit->functions.ptr,
				      (_jit->functions.length + 16) *
				      sizeof(jit_function_t));
	memset(_jit->functions.ptr + _jit->functions.length, 0,
	       16 * sizeof(jit_function_t));
	_jit->functions.length += 16;
    }
    _jit->function = _jit->functions.ptr + _jit->functions.offset++;
    _jit->function->self.size = stack_framesize;
    _jit->function->self.argi = _jit->function->self.argf =
	_jit->function->self.aoff = _jit->function->self.alen = 0;
    _jit->function->regoff = calloc(_jit->reglen, sizeof(jit_int32_t));

    _jit->function->prolog = jit_new_node_no_link(jit_code_prolog);
    jit_link(_jit->function->prolog);
    _jit->function->prolog->w.w = offset;
    _jit->function->epilog = jit_new_node_no_link(jit_code_epilog);
    /*	u:	label value
     *	v:	offset in blocks vector
     *	w:	offset in functions vector
     */
    _jit->function->epilog->w.w = offset;

    jit_regset_new(_jit->function->regset);
}

void
_jit_ellipsis(jit_state_t *_jit)
{
    _jit->function->call.kind = jit_call_varargs;
}

jit_int32_t
_jit_allocai(jit_state_t *_jit, jit_int32_t length)
{
    assert(_jit->function);
    switch (length) {
	case 0:	case 1:						break;
	case 2:		_jit->function->self.aoff &= -2;	break;
	case 3:	case 4:	_jit->function->self.aoff &= -4;	break;
	default:	_jit->function->self.aoff &= -8;	break;
    }
    _jit->function->self.aoff -= length;
    return (_jit->function->self.aoff);
}

void
_jit_ret(jit_state_t *_jit)
{
    jit_node_t		*instr;

    assert(_jit->function);

    /* jump to epilog */
    instr = jit_jmpi();
    jit_patch_at(instr, _jit->function->epilog);
}

void
_jit_retr(jit_state_t *_jit, jit_int32_t u)
{
    jit_movr(JIT_RET, u);
    jit_ret();
}

void
_jit_reti(jit_state_t *_jit, jit_word_t u)
{
    jit_movi(JIT_RET, u);
    jit_ret();
}

void
_jit_retr_f(jit_state_t *_jit, jit_int32_t u)
{
    jit_movr_f(JIT_FRET, u);
    jit_ret();
}

void
_jit_reti_f(jit_state_t *_jit, jit_float32_t u)
{
    jit_movi_f(JIT_FRET, u);
    jit_ret();
}

void
_jit_retr_d(jit_state_t *_jit, jit_int32_t u)
{
    jit_movr_d(JIT_FRET, u);
    jit_ret();
}

void
_jit_reti_d(jit_state_t *_jit, jit_float64_t u)
{
    jit_movi_d(JIT_FRET, u);
    jit_ret();
}

/* must be called internally only */
void
_jit_epilog(jit_state_t *_jit)
{
    assert(_jit->function);

    _jit->function->stack = ((/* first 16 bytes must be allocated */
			      (_jit->function->self.alen > 16 ?
			       _jit->function->self.alen : 16) -
			      /* align stack at 8 bytes */
			      _jit->function->self.aoff) + 7) & -8;
    assert(_jit->function->epilog->next == NULL);
    jit_link(_jit->function->epilog);
    _jit->function = NULL;
}

jit_int32_t
_jit_arg(jit_state_t *_jit)
{
    jit_int32_t		offset;

    assert(_jit->function);
    if (_jit->function->self.argf) {
	_jit->function->self.argi = _jit->function->self.argf;
	_jit->function->self.argf = 0;
    }
    if (_jit->function->self.argi < 4)
	offset = _jit->function->self.argi++;
    else
	offset = _jit->function->self.size;
    _jit->function->self.size += sizeof(jit_word_t);
    return (offset);
}

jit_bool_t
_jit_arg_reg_p(jit_state_t *_jit, jit_int32_t offset)
{
    return (offset >= 0 && offset < 4);
}

jit_int32_t
_jit_arg_f(jit_state_t *_jit)
{
    jit_int32_t		offset;

    assert(_jit->function);
    offset = (_jit->function->self.size - stack_framesize) >> 2;
    if (offset < 4) {
	if (!_jit->function->self.argi) {
	    offset += 4;
	    _jit->function->self.argf += 2;
	}
	else
	    _jit->function->self.argi += 2;
    }
    else
	offset = _jit->function->self.size;
    _jit->function->self.size += sizeof(jit_float32_t);
    return (offset);
}

jit_bool_t
_jit_arg_f_reg_p(jit_state_t *_jit, jit_int32_t offset)
{
    /* 0-3 integer register, 4-7 float register */
    return (offset >= 0 && offset < 8);
}

jit_int32_t
_jit_arg_d(jit_state_t *_jit)
{
    jit_int32_t		offset;

    assert(_jit->function);
    if (_jit->function->self.size & 7) {
	_jit->function->self.size += 4;
	if (_jit->function->self.size < 16 && !_jit->function->self.argi) {
	    _jit->function->self.argi = _jit->function->self.argf;
	    _jit->function->self.argf = 0;
	}
    }
    offset = (_jit->function->self.size - stack_framesize) >> 2;
    if (offset < 4) {
	if (!_jit->function->self.argi) {
	    offset += 4;
	    _jit->function->self.argf += 2;
	}
	else
	    _jit->function->self.argi += 2;
    }
    else
	offset = _jit->function->self.size;
    _jit->function->self.size += sizeof(jit_float64_t);
    return (offset);
}

jit_bool_t
_jit_arg_d_reg_p(jit_state_t *_jit, jit_int32_t offset)
{
    return (jit_arg_f_reg_p(offset));
}

void
_jit_getarg_c(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_extr_c(u, _A0 - v);
    else
	jit_ldxi_c(u, _FP, v);
}

void
_jit_getarg_uc(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_extr_uc(u, _A0 - v);
    else
	jit_ldxi_uc(u, _FP, v);
}

void
_jit_getarg_s(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_extr_s(u, _A0 - v);
    else
	jit_ldxi_s(u, _FP, v);
}

void
_jit_getarg_us(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_extr_us(u, _A0 - v);
    else
	jit_ldxi_us(u, _FP, v);
}

void
_jit_getarg_i(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4) {
#if __WORDSIZE == 64
	jit_extr_i(u, _A0 - v);
#else
	jit_movr(u, _A0 - v);
#endif
    }
    else
	jit_ldxi_i(u, _FP, v);
}

#if __WORDSIZE == 64
void
_jit_getarg_ui(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_extr_ui(u, _A0 - v);
    else
	jit_ldxi_ui(u, _FP, v);
}

void
_jit_getarg_l(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_movr(u, _A0 - v);
    else
	jit_ldxi_l(u, _FP, v);
}
#endif

void
_jit_getarg_f(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_new_node_ww(jit_code_getarg_f, u, _A0 - (v >> 1));
    else if (v < 8)
	jit_movr_f(u, _F12 - ((v - 4) >> 1));
    else
	jit_ldxi_f(u, _FP, v);
}

void
_jit_getarg_d(jit_state_t *_jit, jit_int32_t u, jit_int32_t v)
{
    if (v < 4)
	jit_new_node_ww(jit_code_getarg_d, u, _A0 - (v >> 1));
    else if (v < 8)
	jit_movr_d(u, _F12 - ((v - 4) >> 1));
    else
	jit_ldxi_d(u, _FP, v);
}

void
_jit_pushargr(jit_state_t *_jit, jit_int32_t u)
{
    jit_word_t		offset;

    assert(_jit->function);
    offset = _jit->function->call.size >> 2;
    if (_jit->function->call.argf)
	_jit->function->call.argf = 0;
    if (offset < 4)
	jit_movr(_A0 - offset, u);
    else
	jit_stxi(_jit->function->call.size, JIT_SP, u);
    _jit->function->call.argi = offset + 1;
    _jit->function->call.size += sizeof(jit_word_t);
}

void
_jit_pushargi(jit_state_t *_jit, jit_word_t u)
{
    jit_int32_t		regno;
    jit_word_t		offset;

    assert(_jit->function);
    offset = _jit->function->call.size >> 2;
    if (_jit->function->call.argf)
	_jit->function->call.argf = 0;
    if (offset < 4)
	jit_movi(_A0 - offset, u);
    else {
	regno = jit_get_reg(jit_class_gpr);
	jit_movi(regno, u);
	jit_stxi(_jit->function->call.size, JIT_SP, regno);
	jit_unget_reg(regno);
    }
    _jit->function->call.argi = offset + 1;
    _jit->function->call.size += sizeof(jit_word_t);
}

void
_jit_pushargr_f(jit_state_t *_jit, jit_int32_t u)
{
    jit_word_t		offset;

    assert(_jit->function);
    offset = _jit->function->call.size >> 2;
    if (offset < 3) {
	if (offset & 1) {
	    ++offset;
	    _jit->function->call.size += 4;
	}
	if (_jit->function->call.argi)
	    jit_new_node_ww(jit_code_pushargr_f, _A0 - offset, u);
	else
	    jit_movr_f(_F12 - (offset >> 1), u);
    }
    else
	jit_stxi_f(_jit->function->call.size, JIT_SP, u);
    if (offset < 3) {
	if (!_jit->function->call.argi)
	    _jit->function->call.argf = offset + 2;
	else
	    _jit->function->call.argi = offset + 2;
    }
    else
	_jit->function->call.argi = offset + 1;
    _jit->function->call.size += sizeof(jit_float32_t);
}

void
_jit_pushargi_f(jit_state_t *_jit, jit_float32_t u)
{
    jit_int32_t		regno;
    jit_word_t		offset;

    assert(_jit->function);
    offset = _jit->function->call.size >> 2;
    if (offset < 3) {
	if (offset & 1) {
	    ++offset;
	    _jit->function->call.size += 4;
	}
	if (_jit->function->call.argi)
	    jit_new_node_ww(jit_code_pushargi_f, _A0 - offset, u);
	else
	    jit_movi_f(_F12 - (offset >> 1), u);
    }
    else {
	assert(_jit->function);
	regno = jit_get_reg(jit_class_fpr);
	jit_movi_f(regno, u);
	jit_stxi_f(_jit->function->call.size, JIT_SP, regno);
	jit_unget_reg(regno);
    }
    if (offset < 3) {
	if (!_jit->function->call.argi)
	    _jit->function->call.argf = offset + 2;
	else
	    _jit->function->call.argi = offset + 2;
    }
    else
	_jit->function->call.argi = offset + 1;
    _jit->function->call.size += sizeof(jit_float32_t);
}

void
_jit_pushargr_d(jit_state_t *_jit, jit_int32_t u)
{
    jit_word_t		offset;

    assert(_jit->function);
    if (_jit->function->call.size & 7)
	_jit->function->call.size += 4;
    offset = _jit->function->call.size >> 2;
    if (offset < 3) {
	if (_jit->function->call.argi)
	    jit_new_node_ww(jit_code_pushargr_d, _A0 - offset, u);
	else
	    jit_movr_d(_F12 - (offset >> 1), u);
    }
    else
	jit_stxi_d(_jit->function->call.size, JIT_SP, u);
    if (offset < 3 && !_jit->function->call.argi)
	_jit->function->call.argf = offset + 2;
    else
	_jit->function->call.argi = offset + 2;
    _jit->function->call.size += sizeof(jit_float64_t);
}

void
_jit_pushargi_d(jit_state_t *_jit, jit_float64_t u)
{
    jit_int32_t		regno;
    jit_word_t		offset;

    assert(_jit->function);
    if (_jit->function->call.size & 7)
	_jit->function->call.size += 4;
    offset = _jit->function->call.size >> 2;
    if (offset < 3) {
	if (_jit->function->call.argi)
	    jit_new_node_ww(jit_code_pushargi_d, _A0 - offset, u);
	else
	    jit_movi_d(_F12 - (offset >> 1), u);
    }
    else {
	regno = jit_get_reg(jit_class_fpr);
	jit_movi_d(regno, u);
	jit_stxi_d(_jit->function->call.size, JIT_SP, regno);
	jit_unget_reg(regno);
    }
    if (offset < 3 && !_jit->function->call.argi)
	_jit->function->call.argf = offset + 2;
    else
	_jit->function->call.argi = offset + 2;
    _jit->function->call.size += sizeof(jit_float64_t);
}

jit_bool_t
_jit_regarg_p(jit_state_t *_jit, jit_node_t *node, jit_int32_t regno)
{
    jit_int32_t		spec;

    spec = jit_class(_rvs[regno].spec);
    if (spec & jit_class_arg) {
	if (spec & jit_class_gpr) {
	    regno = _A0 - regno;
	    if (regno >= 0 && regno < node->v.w)
		return (1);
	}
	else if (spec & jit_class_fpr) {
	    regno = _F12 - regno;
	    if (regno >= 0 && regno < node->v.w)
		return (1);
	}
    }

    return (0);
}

void
_jit_finishr(jit_state_t *_jit, jit_int32_t r0)
{
    jit_node_t		*call;

    assert(_jit->function);
    if (_jit->function->self.alen < _jit->function->call.size)
	_jit->function->self.alen = _jit->function->call.size;
    jit_movr(_T9, r0);
    call = jit_callr(_T9);
    call->v.w = _jit->function->self.argi;
    call->w.w = _jit->function->self.argf;
    _jit->function->call.argi = _jit->function->call.argf =
	_jit->function->call.size = 0;
}

jit_node_t *
_jit_finishi(jit_state_t *_jit, jit_pointer_t i0)
{
    jit_node_t		*call;
    jit_node_t		*node;

    assert(_jit->function);
    if (_jit->function->self.alen < _jit->function->call.size)
	_jit->function->self.alen = _jit->function->call.size;
    node = jit_movi(_T9, (jit_word_t)i0);
    call = jit_callr(_T9);
    call->v.w = _jit->function->call.argi;
    call->w.w = _jit->function->call.argf;
    _jit->function->call.argi = _jit->function->call.argf =
	_jit->function->call.size = 0;

    return (node);
}

void
_jit_retval_c(jit_state_t *_jit, jit_int32_t r0)
{
    jit_extr_c(r0, JIT_RET);
}

void
_jit_retval_uc(jit_state_t *_jit, jit_int32_t r0)
{
    jit_extr_uc(r0, JIT_RET);
}

void
_jit_retval_s(jit_state_t *_jit, jit_int32_t r0)
{
    jit_extr_s(r0, JIT_RET);
}

void
_jit_retval_us(jit_state_t *_jit, jit_int32_t r0)
{
    jit_extr_us(r0, JIT_RET);
}

void
_jit_retval_i(jit_state_t *_jit, jit_int32_t r0)
{
#if __WORDSIZE == 32
    if (r0 != JIT_RET)
	jit_movr(r0, JIT_RET);
#else
    jit_extr_i(r0, JIT_RET);
#endif
}

#if __WORDSIZE == 64
void
_jit_retval_ui(jit_state_t *_jit, jit_int32_t r0)
{
    jit_extr_ui(r0, JIT_RET);
}

void
_jit_retval_l(jit_state_t *_jit, jit_int32_t r0)
{
    if (r0 != JIT_RET)
	jit_movr(r0, JIT_RET);
}
#endif

void
_jit_retval_f(jit_state_t *_jit, jit_int32_t r0)
{
    if (r0 != JIT_FRET)
	jit_movr_f(r0, JIT_FRET);
}

void
_jit_retval_d(jit_state_t *_jit, jit_int32_t r0)
{
    if (r0 != JIT_FRET)
	jit_movr_d(r0, JIT_FRET);
}

jit_pointer_t
_jit_emit(jit_state_t *_jit)
{
    jit_node_t		*node;
    jit_node_t		*temp;
    jit_word_t		 word;
    jit_int32_t		 value;
    jit_int32_t		 offset;
    struct {
	jit_node_t	*node;
	jit_word_t	 word;
	jit_int32_t	 patch_offset;
    } undo;

    if (_jit->function)
	jit_epilog();
    jit_optimize();

    _jit->emit = 1;

    _jit->code.length = 16 * 1024 * 1024;
    _jit->code.ptr = mmap(NULL, _jit->code.length,
			  PROT_EXEC | PROT_READ | PROT_WRITE,
			  MAP_PRIVATE | MAP_ANON, -1, 0);
    assert(_jit->code.ptr != MAP_FAILED);
    _jit->pc.uc = _jit->code.ptr;

    /* clear jit_flag_patch from label nodes if reallocating buffer
     * and starting over
     */

    _jit->function = NULL;

    jit_reglive_setup();

    undo.word = 0;
    undo.node = NULL;
    undo.patch_offset = 0;
#define case_rr(name, type)						\
	    case jit_code_##name##r##type:				\
		name##r##type(rn(node->u.w), rn(node->v.w));		\
		break
#define case_rw(name, type)						\
	    case jit_code_##name##i##type:				\
		name##i##type(rn(node->u.w), node->v.w);		\
		break
#define case_wr(name, type)						\
	    case jit_code_##name##i##type:				\
		name##i##type(node->u.w, rn(node->v.w));		\
		break
#define case_rrr(name, type)						\
	    case jit_code_##name##r##type:				\
		name##r##type(rn(node->u.w),				\
			      rn(node->v.w), rn(node->w.w));		\
		break
#define case_rrw(name, type)						\
	    case jit_code_##name##i##type:				\
		name##i##type(rn(node->u.w), rn(node->v.w), node->w.w);	\
		break
#define case_rrf(name, type, size)					\
	    case jit_code_##name##i##type:				\
		assert(node->flag & jit_flag_data);			\
		name##i##type(rn(node->u.w), rn(node->v.w),		\
			      (jit_float##size##_t *)node->w.n->u.w);	\
		break
#define case_wrr(name, type)						\
	    case jit_code_##name##i##type:				\
		name##i##type(node->u.w, rn(node->v.w), rn(node->w.w));	\
		break
#define case_brr(name, type)						\
	    case jit_code_##name##r##type:				\
		temp = node->u.n;					\
		assert(temp->code == jit_code_label ||			\
		       temp->code == jit_code_epilog);			\
		if (temp->flag & jit_flag_patch)			\
		    name##r##type(temp->u.w, rn(node->v.w),		\
				  rn(node->w.w));			\
		else {							\
		    word = name##r##type(_jit->pc.w,			\
					 rn(node->v.w), rn(node->w.w));	\
		    patch(word, node);					\
		}							\
		break
#define case_brw(name, type)						\
	    case jit_code_##name##i##type:				\
		temp = node->u.n;					\
		assert(temp->code == jit_code_label ||			\
		       temp->code == jit_code_epilog);			\
		if (temp->flag & jit_flag_patch)			\
		    name##i##type(temp->u.w,				\
				  rn(node->v.w), node->w.w);		\
		else {							\
		    word = name##i##type(_jit->pc.w,			\
					 rn(node->v.w), node->w.w);	\
		    patch(word, node);					\
		}							\
		break
#define case_brf(name, type, size)					\
	    case jit_code_##name##i##type:				\
		temp = node->u.n;					\
		assert(temp->code == jit_code_label ||			\
		       temp->code == jit_code_epilog);			\
		if (temp->flag & jit_flag_patch)			\
		    name##i##type(temp->u.w, rn(node->v.w),		\
				(jit_float##size##_t *)node->w.n->u.w);	\
		else {							\
		    word = name##i##type(_jit->pc.w, rn(node->v.w),	\
				(jit_float##size##_t *)node->w.n->u.w);	\
		    patch(word, node);					\
		}							\
		break
    for (node = _jit->head; node; node = node->next) {
	value = jit_classify(node->code);
	jit_regarg_set(node, value);
	switch (node->code) {
	    case jit_code_note:
		node->u.w = _jit->pc.w;
		break;
	    case jit_code_label:
		/* remember label is defined */
		node->flag |= jit_flag_patch;
		node->u.w = _jit->pc.w;
		break;
		case_rrr(add,);
		case_rrw(add,);
		case_rrr(addc,);
		case_rrw(addc,);
		case_rrr(addx,);
		case_rrw(addx,);
		case_rrr(sub,);
		case_rrw(sub,);
		case_rrr(subc,);
		case_rrw(subc,);
		case_rrr(subx,);
		case_rrw(subx,);
		case_rrr(mul,);
		case_rrw(mul,);
		case_rrr(div,);
		case_rrw(div,);
		case_rrr(div, _u);
		case_rrw(div, _u);
		case_rrr(rem,);
		case_rrw(rem,);
		case_rrr(rem, _u);
		case_rrw(rem, _u);
		case_rrr(lsh,);
		case_rrw(lsh,);
		case_rrr(rsh,);
		case_rrw(rsh,);
		case_rrr(rsh, _u);
		case_rrw(rsh, _u);
		case_rrr(and,);
		case_rrw(and,);
		case_rrr(or,);
		case_rrw(or,);
		case_rrr(xor,);
		case_rrw(xor,);
		case_rr(trunc, _f_i);
		case_rr(trunc, _d_i);
#if __WORDSIZE == 64
		case_rr(trunc, _f_l);
		case_rr(trunc, _d_l);
#endif
		case_rr(ld, _c);
		case_rw(ld, _c);
		case_rr(ld, _uc);
		case_rw(ld, _uc);
		case_rr(ld, _s);
		case_rw(ld, _s);
		case_rr(ld, _us);
		case_rw(ld, _us);
		case_rr(ld, _i);
		case_rw(ld, _i);
#if __WORDSIZE == 64
		case_rr(ld, _ui);
		case_rw(ld, _ui);
		case_rr(ld, _l);
		case_rw(ld, _l);
#endif
		case_rrr(ldx, _c);
		case_rrw(ldx, _c);
		case_rrr(ldx, _uc);
		case_rrw(ldx, _uc);
		case_rrr(ldx, _s);
		case_rrw(ldx, _s);
		case_rrr(ldx, _us);
		case_rrw(ldx, _us);
		case_rrr(ldx, _i);
		case_rrw(ldx, _i);
#if __WORDSIZE == 64
		case_rrr(ldx, _ui);
		case_rrw(ldx, _ui);
		case_rrr(ldx, _l);
		case_rrw(ldx, _l);
#endif
		case_rr(st, _c);
		case_wr(st, _c);
		case_rr(st, _s);
		case_wr(st, _s);
		case_rr(st, _i);
		case_wr(st, _i);
#if __WORDSIZE == 64
		case_rr(st, _l);
		case_rw(st, _l);
#endif
		case_rrr(stx, _c);
		case_wrr(stx, _c);
		case_rrr(stx, _s);
		case_wrr(stx, _s);
		case_rrr(stx, _i);
		case_wrr(stx, _i);
#if __WORDSIZE == 64
		case_rrr(stx, _l);
		case_wrr(stx, _l);
#endif
		case_rr(hton,);
		case_rr(ext, _c);
		case_rr(ext, _uc);
		case_rr(ext, _s);
		case_rr(ext, _us);
		case_rr(mov,);
	    case jit_code_movi:
		if (node->flag & jit_flag_node) {
		    temp = node->v.n;
		    if (temp->code == jit_code_data ||
			(temp->code == jit_code_label &&
			 (temp->flag & jit_flag_patch)))
			movi(rn(node->u.w), temp->u.w);
		    else {
			assert(temp->code == jit_code_label ||
			       temp->code == jit_code_epilog);
			word = movi_p(rn(node->u.w), node->v.w);
			patch(word, node);
		    }
		}
		else
		    movi(rn(node->u.w), node->v.w);
		break;
		case_rr(neg,);
		case_rr(com,);
		case_rrr(lt,);
		case_rrw(lt,);
		case_rrr(lt, _u);
		case_rrw(lt, _u);
		case_rrr(le,);
		case_rrw(le,);
		case_rrr(le, _u);
		case_rrw(le, _u);
		case_rrr(eq,);
		case_rrw(eq,);
		case_rrr(ge,);
		case_rrw(ge,);
		case_rrr(ge, _u);
		case_rrw(ge, _u);
		case_rrr(gt,);
		case_rrw(gt,);
		case_rrr(gt, _u);
		case_rrw(gt, _u);
		case_rrr(ne,);
		case_rrw(ne,);
		case_brr(blt,);
		case_brw(blt,);
		case_brr(blt, _u);
		case_brw(blt, _u);
		case_brr(ble,);
		case_brw(ble,);
		case_brr(ble, _u);
		case_brw(ble, _u);
		case_brr(beq,);
		case_brw(beq,);
		case_brr(bge,);
		case_brw(bge,);
		case_brr(bge, _u);
		case_brw(bge, _u);
		case_brr(bgt,);
		case_brw(bgt,);
		case_brr(bgt, _u);
		case_brw(bgt, _u);
		case_brr(bne,);
		case_brw(bne,);
		case_brr(boadd,);
		case_brw(boadd,);
		case_brr(boadd, _u);
		case_brw(boadd, _u);
		case_brr(bxadd,);
		case_brw(bxadd,);
		case_brr(bxadd, _u);
		case_brw(bxadd, _u);
		case_brr(bosub,);
		case_brw(bosub,);
		case_brr(bosub, _u);
		case_brw(bosub, _u);
		case_brr(bxsub,);
		case_brw(bxsub,);
		case_brr(bxsub, _u);
		case_brw(bxsub, _u);
		case_brr(bms,);
		case_brw(bms,);
		case_brr(bmc,);
		case_brw(bmc,);
		case_rrr(add, _f);
		case_rrf(add, _f, 32);
		case_rrr(sub, _f);
		case_rrf(sub, _f, 32);
		case_rrr(mul, _f);
		case_rrf(mul, _f, 32);
		case_rrr(div, _f);
		case_rrf(div, _f, 32);
		case_rr(abs, _f);
		case_rr(neg, _f);
		case_rr(sqrt, _f);
		case_rr(ext, _f);
		case_rr(ld, _f);
		case_rw(ld, _f);
		case_rrr(ldx, _f);
		case_rrw(ldx, _f);
		case_rr(st, _f);
		case_wr(st, _f);
		case_rrr(stx, _f);
		case_wrr(stx, _f);
		case_rr(mov, _f);
	    case jit_code_movi_f:
		assert(node->flag & jit_flag_data);
		movi_f(rn(node->u.w), (jit_float32_t *)node->v.n->u.w);
		break;
		case_rr(ext, _d_f);
		case_rrr(lt, _f);
		case_rrf(lt, _f, 32);
		case_rrr(le, _f);
		case_rrf(le, _f, 32);
		case_rrr(eq, _f);
		case_rrf(eq, _f, 32);
		case_rrr(ge, _f);
		case_rrf(ge, _f, 32);
		case_rrr(gt, _f);
		case_rrf(gt, _f, 32);
		case_rrr(ne, _f);
		case_rrf(ne, _f, 32);
		case_rrr(unlt, _f);
		case_rrf(unlt, _f, 32);
		case_rrr(unle, _f);
		case_rrf(unle, _f, 32);
		case_rrr(uneq, _f);
		case_rrf(uneq, _f, 32);
		case_rrr(unge, _f);
		case_rrf(unge, _f, 32);
		case_rrr(ungt, _f);
		case_rrf(ungt, _f, 32);
		case_rrr(ltgt, _f);
		case_rrf(ltgt, _f, 32);
		case_rrr(ord, _f);
		case_rrf(ord, _f, 32);
		case_rrr(unord, _f);
		case_rrf(unord, _f, 32);
		case_brr(blt, _f);
		case_brf(blt, _f, 32);
		case_brr(ble, _f);
		case_brf(ble, _f, 32);
		case_brr(beq, _f);
		case_brf(beq, _f, 32);
		case_brr(bge, _f);
		case_brf(bge, _f, 32);
		case_brr(bgt, _f);
		case_brf(bgt, _f, 32);
		case_brr(bne, _f);
		case_brf(bne, _f, 32);
		case_brr(bunlt, _f);
		case_brf(bunlt, _f, 32);
		case_brr(bunle, _f);
		case_brf(bunle, _f, 32);
		case_brr(buneq, _f);
		case_brf(buneq, _f, 32);
		case_brr(bunge, _f);
		case_brf(bunge, _f, 32);
		case_brr(bungt, _f);
		case_brf(bungt, _f, 32);
		case_brr(bltgt, _f);
		case_brf(bltgt, _f, 32);
		case_brr(bord, _f);
		case_brf(bord, _f, 32);
		case_brr(bunord, _f);
		case_brf(bunord, _f, 32);
		case_rrr(add, _d);
		case_rrf(add, _d, 64);
		case_rrr(sub, _d);
		case_rrf(sub, _d, 64);
		case_rrr(mul, _d);
		case_rrf(mul, _d, 64);
		case_rrr(div, _d);
		case_rrf(div, _d, 64);
		case_rr(abs, _d);
		case_rr(neg, _d);
		case_rr(sqrt, _d);
		case_rr(ext, _d);
		case_rr(ld, _d);
		case_rw(ld, _d);
		case_rrr(ldx, _d);
		case_rrw(ldx, _d);
		case_rr(st, _d);
		case_wr(st, _d);
		case_rrr(stx, _d);
		case_wrr(stx, _d);
		case_rr(mov, _d);
	    case jit_code_movi_d:
		assert(node->flag & jit_flag_data);
		movi_d(rn(node->u.w), (jit_float64_t *)node->v.n->u.w);
		break;
		case_rr(ext, _f_d);
		case_rrr(lt, _d);
		case_rrf(lt, _d, 64);
		case_rrr(le, _d);
		case_rrf(le, _d, 64);
		case_rrr(eq, _d);
		case_rrf(eq, _d, 64);
		case_rrr(ge, _d);
		case_rrf(ge, _d, 64);
		case_rrr(gt, _d);
		case_rrf(gt, _d, 64);
		case_rrr(ne, _d);
		case_rrf(ne, _d, 64);
		case_rrr(unlt, _d);
		case_rrf(unlt, _d, 64);
		case_rrr(unle, _d);
		case_rrf(unle, _d, 64);
		case_rrr(uneq, _d);
		case_rrf(uneq, _d, 64);
		case_rrr(unge, _d);
		case_rrf(unge, _d, 64);
		case_rrr(ungt, _d);
		case_rrf(ungt, _d, 64);
		case_rrr(ltgt, _d);
		case_rrf(ltgt, _d, 64);
		case_rrr(ord, _d);
		case_rrf(ord, _d, 64);
		case_rrr(unord, _d);
		case_rrf(unord, _d, 64);
		case_brr(blt, _d);
		case_brf(blt, _d, 64);
		case_brr(ble, _d);
		case_brf(ble, _d, 64);
		case_brr(beq, _d);
		case_brf(beq, _d, 64);
		case_brr(bge, _d);
		case_brf(bge, _d, 64);
		case_brr(bgt, _d);
		case_brf(bgt, _d, 64);
		case_brr(bne, _d);
		case_brf(bne, _d, 64);
		case_brr(bunlt, _d);
		case_brf(bunlt, _d, 64);
		case_brr(bunle, _d);
		case_brf(bunle, _d, 64);
		case_brr(buneq, _d);
		case_brf(buneq, _d, 64);
		case_brr(bunge, _d);
		case_brf(bunge, _d, 64);
		case_brr(bungt, _d);
		case_brf(bungt, _d, 64);
		case_brr(bltgt, _d);
		case_brf(bltgt, _d, 64);
		case_brr(bord, _d);
		case_brf(bord, _d, 64);
		case_brr(bunord, _d);
		case_brf(bunord, _d, 64);
	    case jit_code_jmpr:
		jmpr(rn(node->u.w));
		break;
	    case jit_code_jmpi:
		temp = node->u.n;
		assert(temp->code == jit_code_label ||
		       temp->code == jit_code_epilog);
		if (temp->flag & jit_flag_patch)
		    jmpi(temp->u.w);
		else {
		    word = jmpi(_jit->pc.w);
		    patch(word, node);
		}
		break;
	    case jit_code_callr:
		callr(rn(node->u.w));
		break;
	    case jit_code_calli:
		if (node->flag & jit_flag_node) {
		    temp = node->u.n;
		    assert(temp->code == jit_code_label ||
			   temp->code == jit_code_epilog);
		    word = calli(temp->u.w);
		    if (!(temp->flag & jit_flag_patch))
			patch(word, node);
		}
		else
		    calli(node->u.w);
		break;
	    case jit_code_prolog:
		_jit->function = _jit->functions.ptr + node->w.w;
		undo.node = node;
		undo.word = _jit->pc.w;
		undo.patch_offset = _jit->patches.offset;
	    restart_function:
		_jit->again = 0;
		prolog(node);
		break;
	    case jit_code_epilog:
		assert(_jit->function == _jit->functions.ptr + node->w.w);
		if (_jit->again) {
		    for (temp = undo.node->next;
			 temp != node; temp = temp->next) {
			if (temp->code == jit_code_label ||
			    temp->code == jit_code_epilog)
			    temp->flag &= ~jit_flag_patch;
		    }
		    node = undo.node;
		    _jit->pc.w = undo.word;
		    _jit->patches.offset = undo.patch_offset;
		    goto restart_function;
		}
		/* remember label is defined */
		node->flag |= jit_flag_patch;
		node->u.w = _jit->pc.w;
		epilog(node);
		_jit->function = NULL;
		break;
	    case jit_code_getarg_f:
		getarg_f(rn(node->u.w), rn(node->v.w));
		break;
		case_rr(pusharg, _f);
	    case jit_code_pushargi_f:
		assert(node->flag & jit_flag_data);
		pushargi_f(rn(node->u.w), (jit_float32_t *)node->v.n->u.w);
		break;
	    case jit_code_getarg_d:
		getarg_d(rn(node->u.w), rn(node->v.w));
		break;
		case_rr(pusharg, _d);
	    case jit_code_pushargi_d:
		assert(node->flag & jit_flag_data);
		pushargi_d(rn(node->u.w), (jit_float64_t *)node->v.n->u.w);
		break;
	    default:
		abort();
	}
	if (jit_carry != _NOREG) {
	    switch (node->code) {
		case jit_code_note:
		case jit_code_addcr:		case jit_code_addci:
		case jit_code_addxr:		case jit_code_addxi:
		case jit_code_subcr:		case jit_code_subci:
		case jit_code_subxr:		case jit_code_subxi:
		    break;
		default:
		    jit_unget_reg(jit_carry);
		    jit_carry = _NOREG;
		    break;
	    }
	}
	jit_regarg_clr(node, value);
	/* update register live state */
	jit_reglive(node);
    }
#undef case_brf
#undef case_brw
#undef case_brr
#undef case_wrr
#undef case_rrf
#undef case_rrw
#undef case_rrr
#undef case_wr
#undef case_rw
#undef case_rr

    for (offset = 0; offset < _jit->patches.offset; offset++) {
	node = _jit->patches.ptr[offset].node;
	word = node->code == jit_code_movi ? node->v.n->u.w : node->u.n->u.w;
	patch_at(_jit->patches.ptr[offset].inst, word);
    }

#if defined(__linux__)
    _flush_cache((char *)_jit->code.ptr, _jit->pc.uc - _jit->code.ptr, ICACHE);
#endif

    return (_jit->code.ptr);
}

#define CODE				1
#  include "jit_mips-cpu.c"
#  include "jit_mips-fpu.c"
#undef CODE

void
_emit_ldxi(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
#if __WORDSIZE == 32
    ldxi_i(rn(r0), rn(r1), i0);
#else
    ldxi_l(rn(r0), rn(r1), i0);
#endif
}

void
_emit_stxi(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
#if __WORDSIZE == 32
    stxi_i(i0, rn(r0), rn(r1));
#else
    stxi_l(i0, rn(r0), rn(r1));
#endif
}

void
_emit_ldxi_d(jit_state_t *_jit, jit_int32_t r0, jit_int32_t r1, jit_word_t i0)
{
    ldxi_d(rn(r0), rn(r1), i0);
}

void
_emit_stxi_d(jit_state_t *_jit, jit_word_t i0, jit_int32_t r0, jit_int32_t r1)
{
    stxi_d(i0, rn(r0), rn(r1));
}

static void
_patch(jit_state_t *_jit, jit_word_t instr, jit_node_t *node)
{
    jit_int32_t		 flag;

    assert(node->flag & jit_flag_node);
    if (node->code == jit_code_movi)
	flag = node->v.n->flag;
    else
	flag = node->u.n->flag;
    assert(!(flag & jit_flag_patch));
    if (_jit->patches.offset >= _jit->patches.length) {
	_jit->patches.ptr = realloc(_jit->patches.ptr,
				    (_jit->patches.length + 1024) *
				    sizeof(jit_patch_t));
	memset(_jit->patches.ptr + _jit->patches.length, 0,
	       1024 * sizeof(jit_patch_t));
	_jit->patches.length += 1024;
    }
    _jit->patches.ptr[_jit->patches.offset].inst = instr;
    _jit->patches.ptr[_jit->patches.offset].node = node;
    ++_jit->patches.offset;
}
