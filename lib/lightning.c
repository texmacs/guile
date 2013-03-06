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

#include <lightning.h>
#include <lightning/jit_private.h>
#include <sys/mman.h>

#define jit_regload_reload		0	/* convert to reload */
#define jit_regload_delete		1	/* just remove node */
#define jit_regload_isdead		2	/* delete and unset live bit */

/*
 * Prototypes
 */
static jit_word_t hash_data(jit_pointer_t, jit_word_t);

#define new_pool()			_new_pool(_jit)
static void _new_pool(jit_state_t*);

#define new_node(u)			_new_node(_jit, u)
static jit_node_t *_new_node(jit_state_t*, jit_code_t);

#define link_node(u)			_link_node(_jit, u)
static inline jit_node_t *_link_node(jit_state_t*, jit_node_t*);

#define del_node(u, v)			_del_node(_jit, u, v)
static inline void _del_node(jit_state_t*, jit_node_t*, jit_node_t*);

#define del_label(u, v)			_del_label(_jit, u, v)
static void _del_label(jit_state_t*, jit_node_t*, jit_node_t*);

#define jit_setup(block)		_jit_setup(_jit, block)
static void
_jit_setup(jit_state_t *_jit, jit_block_t *block);

#define jit_update(node, live, mask)	_jit_update(_jit, node, live, mask)
static void
_jit_update(jit_state_t *_jit, jit_node_t *node,
	    jit_regset_t *live, jit_regset_t *mask);

#define thread_jumps()			_thread_jumps(_jit)
static void
_thread_jumps(jit_state_t *_jit);

#define sequential_labels()		_sequential_labels(_jit)
static void
_sequential_labels(jit_state_t *_jit);

#define shortcut_jump(prev, node)	_shortcut_jump(_jit, prev, node)
static jit_bool_t
_shortcut_jump(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node);

#define redundant_jump(prev, node)	_redundant_jump(_jit, prev, node)
static jit_bool_t
_redundant_jump(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node);

static jit_code_t
reverse_jump_code(jit_code_t code);

#define reverse_jump(prev, node)	_reverse_jump(_jit, prev, node)
static jit_bool_t
_reverse_jump(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node);

#define redundant_store(node, jump)	_redundant_store(_jit, node, jump)
static void
_redundant_store(jit_state_t *_jit, jit_node_t *node, jit_bool_t jump);

#define simplify_movr(p, n, k, s)	_simplify_movr(_jit, p, n, k, s)
static jit_bool_t
_simplify_movr(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node,
	       jit_int32_t kind, jit_int32_t size);

#define simplify_movi(p, n, k, s)	_simplify_movi(_jit, p, n, k, s)
static jit_bool_t
_simplify_movi(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node,
	       jit_int32_t kind, jit_int32_t size);

#define simplify_ldxi(prev, node)	_simplify_ldxi(_jit, prev, node)
static jit_bool_t
_simplify_ldxi(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node);

#define simplify_stxi(prev, node)	_simplify_stxi(_jit, prev, node)
static jit_bool_t
_simplify_stxi(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node);

#define simplify_spill(node, regno)	_simplify_spill(_jit, node, regno)
static void
_simplify_spill(jit_state_t *_jit, jit_node_t *node, jit_int32_t regno);

#define simplify()			_simplify(_jit)
static void
_simplify(jit_state_t *_jit);

#define jit_reg_undef			-1
#define jit_reg_static			 0
#define jit_reg_change			 1
#define register_change_p(n, l, r)	_register_change_p(_jit, n, l, r)
static jit_int32_t
_register_change_p(jit_state_t *_jit, jit_node_t *node, jit_node_t *link,
		   jit_int32_t regno);

#define spill_reglive_p(node, regno)	_spill_reglive_p(_jit, node, regno)
static jit_bool_t
_spill_reglive_p(jit_state_t *_jit, jit_node_t *node, jit_int32_t regno);

#define patch_registers()		_patch_registers(_jit)
static void
_patch_registers(jit_state_t *_jit);

#define patch_register(n,l,r,p)		_patch_register(_jit,n,l,r,p)
static void
_patch_register(jit_state_t *jit, jit_node_t *node, jit_node_t *link,
		jit_int32_t regno, jit_int32_t patch);

/*
 * Initialization
 */
const char	*jit_progname;

/*
 * Implementation
 */
void
init_jit(char *progname)
{
    jit_progname = progname;
    jit_get_cpu();
    jit_init_debug();
}

void
finish_jit(void)
{
    jit_finish_debug();
}

jit_int32_t
_jit_get_reg(jit_state_t *_jit, jit_int32_t regspec)
{
    jit_int32_t		spec;
    jit_int32_t		regno;

    spec = regspec & ~(jit_class_chk|jit_class_nospill);
    if (spec & jit_class_named) {
	regno = jit_regno(spec);
	if (jit_regset_tstbit(_jit->regsav, regno))
	    /* fail if register is spilled */
	    goto fail;
	if (jit_regset_tstbit(_jit->regarg, regno))
	    /* fail if register is an argument to current instruction */
	    goto fail;
	if (jit_regset_tstbit(_jit->reglive, regno)) {
	    if (regspec & jit_class_nospill)
		/* fail if register is live and should not spill/reload */
		goto fail;
	    goto spill;
	}
	jit_regset_setbit(_jit->regarg, regno);
	return (regno);
    }
    else
	assert(jit_class(spec) != 0);

    if (_jit->emit) {
	/* search for a free register matching spec */
	for (regno = 0; regno < _jit->reglen; regno++) {
	    if ((jit_class(_rvs[regno].spec) & spec) == spec &&
		!jit_regset_tstbit(_jit->regarg, regno) &&
		!jit_regset_tstbit(_jit->reglive, regno))
		goto regarg;
	}

	/* search for a register matching spec that is not an argument
	 * for the current instruction */
	for (regno = 0; regno < _jit->reglen; regno++) {
	    if ((jit_class(_rvs[regno].spec) & spec) == spec &&
		!jit_regset_tstbit(_jit->regsav, regno) &&
		!jit_regset_tstbit(_jit->regarg, regno) &&
		!(regspec & jit_class_nospill)) {
	    spill:
		assert(_jit->function);
		if (spec & jit_class_gpr) {
		    if (!_jit->function->regoff[regno]) {
			_jit->function->regoff[regno] =
			    jit_allocai(sizeof(jit_word_t));
			_jit->again = 1;
		    }
		    emit_stxi(_jit->function->regoff[regno], JIT_FP, regno);
		}
		else {
		    if (!_jit->function->regoff[regno]) {
			_jit->function->regoff[regno] =
			    jit_allocai(sizeof(jit_float64_t));
			_jit->again = 1;
		    }
		    emit_stxi_d(_jit->function->regoff[regno], JIT_FP, regno);
		}
		jit_regset_setbit(_jit->regsav, regno);
	    regarg:
		jit_regset_setbit(_jit->regarg, regno);
		if (jit_class(_rvs[regno].spec) & jit_class_sav) {
		    /* if will modify callee save registers without a
		     * function prolog, better patch this assertion */
		    assert(_jit->function);
		    if (!jit_regset_tstbit(_jit->function->regset, regno)) {
			jit_regset_setbit(_jit->function->regset, regno);
			_jit->again = 1;
		    }
		}
		return (regno);
	    }
	}
    }
    else {
	/* nospill hint only valid during emit" */
	assert(!(regspec & jit_class_nospill));
	for (regno = 0; regno < _jit->reglen; regno++) {
	    if ((jit_class(_rvs[regno].spec) & spec) == spec &&
		!jit_regset_tstbit(_jit->regsav, regno) &&
		!jit_regset_tstbit(_jit->regarg, regno)) {
		jit_regset_setbit(_jit->regarg, regno);
		jit_regset_setbit(_jit->regsav, regno);
		jit_save(regno);
		return (jit_regno_patch|regno);
	    }
	}
    }

    /* Out of hardware registers */
fail:
    assert(regspec & jit_class_chk);
    return (JIT_NOREG);
}

void
_jit_unget_reg(jit_state_t *_jit, jit_int32_t regno)
{
    regno = jit_regno(regno);
    if (jit_regset_tstbit(_jit->regsav, regno)) {
	if (_jit->emit) {
	    if (jit_class(_rvs[regno].spec) & jit_class_gpr)
		emit_ldxi(regno, JIT_FP, _jit->function->regoff[regno]);
	    else
		emit_ldxi_d(regno, JIT_FP, _jit->function->regoff[regno]);
	}
	else
	    jit_load(regno);
	jit_regset_clrbit(_jit->regsav, regno);
    }
    assert(jit_regset_tstbit(_jit->regarg, regno));
    jit_regset_clrbit(_jit->regarg, regno);
}

unsigned long
jit_regset_scan1(jit_regset_t set, jit_int32_t offset)
{
    assert(offset >= 0 && offset <= 63);
    for (; offset < 64; offset++) {
	if (set & (1LL << offset))
	    return (offset);
    }
    return (ULONG_MAX);
}

void
_jit_save(jit_state_t *_jit, jit_int32_t reg)
{
    reg = jit_regno(reg);
    assert(!_jit->emit);
    _jit->spill[reg] = jit_new_node_w(jit_code_save, reg);
}

void
_jit_load(jit_state_t *_jit, jit_int32_t reg)
{
    jit_node_t		*node;

    reg = jit_regno(reg);
    assert(!_jit->emit);
    assert(_jit->spill[reg]);
    node = jit_new_node_w(jit_code_load, reg);
    /* create a path to flag the save/load is not required */
    node->link = _jit->spill[reg];
    node->link->link = node;
    _jit->spill[reg] = NULL;
}

static jit_word_t
hash_data(jit_pointer_t data, jit_word_t length)
{
    jit_uint8_t		*ptr;
    jit_word_t		 i, key;
    for (i = key = 0, ptr = data; i < length; i++)
	key = (key << (key & 1)) ^ ptr[i];
    return (key);
}

jit_pointer_t
_jit_address(jit_state_t *_jit, jit_node_t *node)
{
    assert(_jit->done);
    assert(node &&
	   (node->code == jit_code_note || node->code == jit_code_name));
    return ((jit_pointer_t)node->u.w);
}

jit_node_t *
_jit_data(jit_state_t *_jit, jit_pointer_t data,
	  jit_word_t length, jit_int32_t align)
{
    jit_word_t		 key;
    jit_node_t		*node;

    assert(!_jit->emit);

    /* Ensure there is space even if asking for a duplicate */
    if (((_jit->data.offset + 7) & -8) + length > _jit->data.length) {
	jit_word_t	 size;

	size = (_jit->data.length + length + 4096) & - 4095;
	assert(size >= _jit->data.length);
	if (_jit->data.ptr == NULL)
	    _jit->data.ptr = calloc(1, size);
	else {
	    _jit->data.ptr = realloc(_jit->data.ptr, size);
	    memset(_jit->data.ptr + _jit->data.length, 0,
		   size - _jit->data.length);
	}
	_jit->data.length = size;
    }
    if (_jit->data.table == NULL)
	_jit->data.table = calloc(_jit->data.size = 16, sizeof(jit_node_t*));

    key = hash_data(data, length) & (_jit->data.size - 1);
    node = _jit->data.table[key];
    for (; node; node = node->next) {
	if (node->v.w == length &&
	    memcmp(_jit->data.ptr + node->u.w, data, length) == 0)
	    break;
    }

    if (!node) {
	node = jit_new_node_no_link(jit_code_data);
	if (!align)
	    align = length;
	switch (align) {
	    case 0:	case 1:
		break;
	    case 2:
		_jit->data.offset = (_jit->data.offset + 1) & -2;
		break;
	    case 3:	case 4:
		_jit->data.offset = (_jit->data.offset + 3) & -4;
		break;
	    default:
		_jit->data.offset = (_jit->data.offset + 7) & -8;
		break;
	}
	node->u.w = _jit->data.offset;
	node->v.w = length;
	memcpy(_jit->data.ptr + _jit->data.offset, data, length);
	_jit->data.offset += length;

	node->next = _jit->data.table[key];
	_jit->data.table[key] = node;
	++_jit->data.count;

	/* Rehash if more than 75% used table */
	if (_jit->data.count >
	    (_jit->data.size >> 1) + (_jit->data.size >> 2) &&
	    (_jit->data.size << 1) > _jit->data.size) {
	    jit_word_t	  i;
	    jit_node_t	**hash;
	    jit_node_t	 *next;
	    jit_node_t	 *temp;

	    hash = calloc(_jit->data.size << 1, sizeof(jit_node_t*));
	    for (i = 0; i < _jit->data.size; i++) {
		temp = _jit->data.table[i];
		for (; temp; temp = next) {
		    next = temp->next;
		    key = hash_data(_jit->data.ptr + temp->u.w, temp->v.w) &
			  ((_jit->data.size << 1) - 1);
		    temp->next = hash[key];
		    hash[key] = temp;
		}
	    }
	    free(_jit->data.table);
	    _jit->data.table = hash;
	    _jit->data.size <<= 1;
	}
    }

    return (node);
}

static void
_new_pool(jit_state_t *_jit)
{
    jit_node_t		*list;
    jit_int32_t		 offset;

    if (_jit->pool.offset >= _jit->pool.length) {
	jit_node_t	**ptr;
	jit_int32_t	  length;

	length = _jit->pool.length + 16;
	ptr = realloc(_jit->pool.ptr, length * sizeof(jit_node_t));
	memset(ptr + _jit->pool.length, 0, 16 * sizeof(jit_node_t));
	_jit->pool.ptr = ptr;
	_jit->pool.length = length;
    }
   _jit->pool.ptr[_jit->pool.offset] = calloc(sizeof(jit_node_t), 1024);
    list = _jit->pool.ptr[_jit->pool.offset];
    for (offset = 1; offset < 1024; offset++, list++)
	list->next = list + 1;
    list->next = _jit->list;
    _jit->list = _jit->pool.ptr[_jit->pool.offset];
    ++_jit->pool.offset;
}

static jit_node_t *
_new_node(jit_state_t *_jit, jit_code_t code)
{
    jit_node_t		*node;

    if (_jit->list == NULL)
	new_pool();
    node = _jit->list;
    _jit->list = node->next;
    node->next = NULL;
    node->code = code;

    return (node);
}

static inline jit_node_t *
_link_node(jit_state_t *_jit, jit_node_t *node)
{
    if (_jit->tail)
	_jit->tail->next = node;
    else
	_jit->head = node;
    return (_jit->tail = node);
}

static inline void
_del_node(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node)
{
    if (prev == node) {
	assert(prev == _jit->head);
	_jit->head = node->next;
    }
    else
	prev->next = node->next;
    memset(node, 0, sizeof(jit_node_t));
    node->next = _jit->list;
    _jit->list = node;
}

static void
_del_label(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node)
{
    jit_block_t		*block;

    /* only allow call to del_label on linked labels */
    block = _jit->blocks.ptr + node->v.w;
    assert(block->label == node);

    /* del_label() should only be called when optimizing.
     * This will leave an empty block index */
    jit_regset_del(block->reglive);
    jit_regset_del(block->regmask);
    block->label = NULL;

    /* redundant, should be already true */
    assert(node->link == NULL);
    del_node(prev, node);
}

jit_state_t *
jit_new_state(void)
{
    jit_state_t		*_jit;

    _jit = calloc(1, sizeof(jit_state_t));
    jit_regset_new(_jit->regarg);
    jit_regset_new(_jit->regsav);
    jit_regset_new(_jit->reglive);
    jit_regset_new(_jit->regmask);
    mpz_init(_jit->blockmask);

    jit_init();

    _jit->spill = calloc(_jit->reglen, sizeof(jit_node_t*));
    _jit->gen = calloc(_jit->reglen, sizeof(jit_int32_t));
    _jit->values = calloc(_jit->reglen, sizeof(jit_value_t));

    _jit->patches.ptr = calloc(_jit->patches.length = 1024,
				sizeof(jit_patch_t));
    _jit->functions.ptr = calloc(_jit->functions.length = 16,
				  sizeof(jit_function_t));
    _jit->pool.ptr = calloc(_jit->pool.length = 16,
			     sizeof(jit_node_t*));
    _jit->blocks.ptr = calloc(_jit->blocks.length = 16,
			       sizeof(jit_block_t));
#if __arm__ && DISASSEMBLER
    _jit->data_info.ptr = calloc(_jit->data_info.length = 1024,
				  sizeof(jit_data_info_t));
#endif

    /* allocate at most one extra note in case jit_name() is
     * never called, or called after adding at least one note */
    _jit->note.length = 1;
    _jit->note.size = sizeof(jit_note_t);

    return (_jit);
}

void
_jit_clear_state(jit_state_t *_jit)
{
    jit_word_t		 offset;
    jit_function_t	*function;

    /* release memory not required at jit execution time and set
     * pointers to NULL to explicitly know they are released */
    _jit->head = _jit->tail = NULL;

    mpz_clear(_jit->blockmask);

    free(_jit->data.table);
    _jit->data.table = NULL;
    _jit->data.size = _jit->data.count = 0;

    free(_jit->spill);
    _jit->spill = NULL;
    free(_jit->gen);
    _jit->gen = NULL;
    free(_jit->values);
    _jit->values = NULL;

    free(_jit->blocks.ptr);
    _jit->blocks.ptr = NULL;

    free(_jit->patches.ptr);
    _jit->patches.ptr = NULL;
    _jit->patches.offset = _jit->patches.length = 0;

    for (offset = 0; offset < _jit->functions.offset; offset++) {
	function = _jit->functions.ptr + offset;
	free(function->regoff);
	function->regoff = NULL;
    }
    free(_jit->functions.ptr);
    _jit->functions.offset = _jit->functions.length = 0;
    _jit->function = NULL;

    for (offset = 0; offset < _jit->pool.length; offset++)
	free(_jit->pool.ptr[offset]);
    free(_jit->pool.ptr);
    _jit->pool.ptr = NULL;
    _jit->pool.offset = _jit->pool.length = 0;
    _jit->list = NULL;

    _jit->note.head = _jit->note.tail =
	_jit->note.name = _jit->note.note = NULL;
    _jit->note.base = NULL;

#if __arm__ && DISASSEMBLER
    free(_jit->data_info.ptr);
    _jit->data_info.ptr = NULL;
#endif
}

void
_jit_destroy_state(jit_state_t *_jit)
{
    munmap(_jit->code.ptr, _jit->code.length);
    munmap(_jit->data.ptr, _jit->data.length);
    free(_jit);
}

jit_node_t *
_jit_new_node(jit_state_t *_jit, jit_code_t code)
{
    assert(!_jit->emit);
    return (link_node(new_node(code)));
}

jit_node_t *
_jit_new_node_no_link(jit_state_t *_jit, jit_code_t code)
{
    assert(!_jit->emit);
    return (new_node(code));
}

void
_jit_link_node(jit_state_t *_jit, jit_node_t *node)
{
    assert(!_jit->emit);
    link_node(node);
}

jit_node_t *
_jit_new_node_w(jit_state_t *_jit, jit_code_t code,
		jit_word_t u)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.w = u;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_p(jit_state_t *_jit, jit_code_t code,
		jit_pointer_t u)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.p = u;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_ww(jit_state_t *_jit, jit_code_t code,
		 jit_word_t u, jit_word_t v)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.w = u;
    node->v.w = v;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_wp(jit_state_t *_jit, jit_code_t code,
		 jit_word_t u, jit_pointer_t v)
{
    return (jit_new_node_ww(code, u, (jit_word_t)v));
}

jit_node_t *
_jit_new_node_pw(jit_state_t *_jit, jit_code_t code,
		 jit_pointer_t u, jit_word_t v)
{
    return (jit_new_node_ww(code, (jit_word_t)u, v));
}

jit_node_t *
_jit_new_node_wf(jit_state_t *_jit, jit_code_t code,
		 jit_word_t u, jit_float32_t v)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.w = u;
    node->v.f = v;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_wd(jit_state_t *_jit, jit_code_t code,
		 jit_word_t u, jit_float64_t v)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.w = u;
    node->v.d = v;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_www(jit_state_t *_jit, jit_code_t code,
		  jit_word_t u, jit_word_t v, jit_word_t w)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.w = u;
    node->v.w = v;
    node->w.w = w;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_qww(jit_state_t *_jit, jit_code_t code,
		  jit_int32_t l, jit_int32_t h,
		  jit_word_t v, jit_word_t w)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    assert(l != h);
    node->u.q.l = l;
    node->u.q.h = h;
    node->v.w = v;
    node->w.w = w;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_wwf(jit_state_t *_jit, jit_code_t code,
		  jit_word_t u, jit_word_t v, jit_float32_t w)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.w = u;
    node->v.w = v;
    node->w.f = w;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_wwd(jit_state_t *_jit, jit_code_t code,
		  jit_word_t u, jit_word_t v, jit_float64_t w)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.w = u;
    node->v.w = v;
    node->w.d = w;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_pww(jit_state_t *_jit, jit_code_t code,
		  jit_pointer_t u, jit_word_t v, jit_word_t w)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.p = u;
    node->v.w = v;
    node->w.w = w;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_pwf(jit_state_t *_jit, jit_code_t code,
		  jit_pointer_t u, jit_word_t v, jit_float32_t w)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.p = u;
    node->v.w = v;
    node->w.f = w;
    return (link_node(node));
}

jit_node_t *
_jit_new_node_pwd(jit_state_t *_jit, jit_code_t code,
		  jit_pointer_t u, jit_word_t v, jit_float64_t w)
{
    jit_node_t		*node = new_node(code);
    assert(!_jit->emit);
    node->u.p = u;
    node->v.w = v;
    node->w.d = w;
    return (link_node(node));
}

jit_node_t *
_jit_label(jit_state_t *_jit)
{
    jit_node_t		*node;

    if (!(node = _jit->tail) || node->code != jit_code_label) {
	node = jit_forward();
	jit_link(node);
    }

    return (node);
}

jit_node_t *
_jit_forward(jit_state_t *_jit)
{
    return (jit_new_node_no_link(jit_code_label));
}

void
_jit_link(jit_state_t *_jit, jit_node_t *node)
{
    jit_block_t		*block;

    assert((node->code == jit_code_label ||
	    node->code == jit_code_prolog ||
	    node->code == jit_code_epilog) && !node->next);
    jit_link_node(node);
    if (_jit->blocks.offset >= _jit->blocks.length) {
	jit_word_t	  length;

	length = _jit->blocks.length + 16;
	block = realloc(_jit->blocks.ptr, length * sizeof(jit_block_t));
	memset(block + _jit->blocks.length, 0, 16 * sizeof(jit_block_t));
	_jit->blocks.ptr = block;
	_jit->blocks.length = length;
    }
    block = _jit->blocks.ptr + _jit->blocks.offset;
    block->label = node;
    node->v.w = _jit->blocks.offset;
    jit_regset_new(block->reglive);
    jit_regset_new(block->regmask);
    ++_jit->blocks.offset;
}

void
_jit_prepare(jit_state_t *_jit)
{
    assert(_jit->function);
    _jit->function->call.call = jit_call_default;
    _jit->function->call.argi =
	_jit->function->call.argf =
	_jit->function->call.size = 0;
    _jit->prepare = 1;
}

/* If declaring a jit function as varargs, in most backends it does
 * not change anything. Currently only exception is arm backend, that
 * if running in hardware float abi, switches to software float abi
 * if "self" function is varargs. Otherwise, there is no logic to
 * handle va_list like objects that need to parse runtime state, and
 * that is mainly because jit_arg* and jit_getarg* work only with
 * constants values, and one must not expect them to be handled at
 * runtime, they are parsed only once (same applies to jit_allocai,
 * that has no jit_allocar counterpart).
 */
void
_jit_ellipsis(jit_state_t *_jit)
{
    if (_jit->prepare) {
	assert(!_jit->function->call.call & jit_call_varargs);
	_jit->function->call.call |= jit_call_varargs;
    }
    else {
	assert(!_jit->function->self.call & jit_call_varargs);
	_jit->function->self.call |= jit_call_varargs;
    }
}

void
_jit_patch(jit_state_t* _jit, jit_node_t *instr)
{
    jit_node_t		*label;

    if (!(label = _jit->tail) || label->code != jit_code_label)
	label = jit_label();
    jit_patch_at(instr, label);
}

jit_int32_t
_jit_classify(jit_state_t *_jit, jit_code_t code)
{
    jit_int32_t		mask;

    switch (code) {
	case jit_code_data:	case jit_code_save:	case jit_code_load:
	case jit_code_name:	case jit_code_label:	case jit_code_note:
	case jit_code_prolog:	case jit_code_epilog:
	    mask = 0;
	    break;
	case jit_code_live:
	    mask = jit_cc_a0_reg;
	    break;
	case jit_code_arg:	case jit_code_arg_f:	case jit_code_arg_d:
	    mask = jit_cc_a0_int;
	    break;
	case jit_code_calli:	case jit_code_jmpi:
	    mask = jit_cc_a0_jmp;
	    break;
	case jit_code_callr:	case jit_code_jmpr:
	    mask = jit_cc_a0_reg|jit_cc_a0_jmp;
	    break;
	case jit_code_x86_retval_f:
	case jit_code_x86_retval_d:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg;
	    break;
	case jit_code_movi:	case jit_code_ldi_c:	case jit_code_ldi_uc:
	case jit_code_ldi_s:	case jit_code_ldi_us:	case jit_code_ldi_i:
	case jit_code_ldi_ui:	case jit_code_ldi_l:	case jit_code_ldi_f:
	case jit_code_ldi_d:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_int;
	    break;
	case jit_code_movi_f:	case jit_code_movi_f_w:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_flt;
	    break;
	case jit_code_movi_d:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_dbl;
	    break;
	case jit_code_movi_d_ww:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_reg|jit_cc_a1_chg|
		   jit_cc_a2_dbl;
	    break;
	case jit_code_negr:	case jit_code_comr:	case jit_code_movr:
	case jit_code_extr_c:	case jit_code_extr_uc:	case jit_code_extr_s:
	case jit_code_extr_us:	case jit_code_extr_i:	case jit_code_extr_ui:
	case jit_code_truncr_f_i:			case jit_code_truncr_f_l:
	case jit_code_truncr_d_i:			case jit_code_truncr_d_l:
	case jit_code_htonr:	case jit_code_ldr_c:	case jit_code_ldr_uc:
	case jit_code_ldr_s:	case jit_code_ldr_us:	case jit_code_ldr_i:
	case jit_code_ldr_ui:	case jit_code_ldr_l:	case jit_code_negr_f:
	case jit_code_absr_f:	case jit_code_sqrtr_f:	case jit_code_movr_f:
	case jit_code_extr_f:	case jit_code_extr_d_f:	case jit_code_ldr_f:
	case jit_code_negr_d:	case jit_code_absr_d:	case jit_code_sqrtr_d:
	case jit_code_movr_d:	case jit_code_extr_d:	case jit_code_extr_f_d:
	case jit_code_ldr_d:
	case jit_code_movr_w_f:	case jit_code_movr_f_w:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_reg;
	    break;
	case jit_code_movr_d_ww:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_reg|jit_cc_a1_chg|
		   jit_cc_a2_reg;
	    break;
	case jit_code_addi:	case jit_code_addxi:	case jit_code_addci:
	case jit_code_subi:	case jit_code_subxi:	case jit_code_subci:
	case jit_code_muli:	case jit_code_divi:	case jit_code_divi_u:
	case jit_code_remi:	case jit_code_remi_u:	case jit_code_andi:
	case jit_code_ori:	case jit_code_xori:	case jit_code_lshi:
	case jit_code_rshi:	case jit_code_rshi_u:	case jit_code_lti:
	case jit_code_lti_u:	case jit_code_lei:	case jit_code_lei_u:
	case jit_code_eqi:	case jit_code_gei:	case jit_code_gei_u:
	case jit_code_gti:	case jit_code_gti_u:	case jit_code_nei:
	case jit_code_ldxi_c:	case jit_code_ldxi_uc:	case jit_code_ldxi_s:
	case jit_code_ldxi_us:	case jit_code_ldxi_i:	case jit_code_ldxi_ui:
	case jit_code_ldxi_l:	case jit_code_ldxi_f:	case jit_code_ldxi_d:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_reg|jit_cc_a2_int;
	    break;
	case jit_code_qmuli:	case jit_code_qmuli_u:
	case jit_code_qdivi:	case jit_code_qdivi_u:
	    mask = jit_cc_a0_reg|jit_cc_a0_rlh|jit_cc_a0_chg|
		   jit_cc_a1_reg|jit_cc_a2_int;
	    break;
	case jit_code_addi_f:	case jit_code_subi_f:	case jit_code_muli_f:
	case jit_code_divi_f:	case jit_code_lti_f:	case jit_code_lei_f:
	case jit_code_eqi_f:	case jit_code_gei_f:	case jit_code_gti_f:
	case jit_code_nei_f:	case jit_code_unlti_f:	case jit_code_unlei_f:
	case jit_code_uneqi_f:	case jit_code_ungei_f:	case jit_code_ungti_f:
	case jit_code_ltgti_f:	case jit_code_ordi_f:	case jit_code_unordi_f:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_reg|jit_cc_a2_flt;
	    break;
	case jit_code_addi_d:	case jit_code_subi_d:	case jit_code_muli_d:
	case jit_code_divi_d:	case jit_code_lti_d:	case jit_code_lei_d:
	case jit_code_eqi_d:	case jit_code_gei_d:	case jit_code_gti_d:
	case jit_code_nei_d:	case jit_code_unlti_d:	case jit_code_unlei_d:
	case jit_code_uneqi_d:	case jit_code_ungei_d:	case jit_code_ungti_d:
	case jit_code_ltgti_d:	case jit_code_ordi_d:	case jit_code_unordi_d:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_reg|jit_cc_a2_dbl;
	    break;
	case jit_code_addr:	case jit_code_addxr:	case jit_code_addcr:
	case jit_code_subr:	case jit_code_subxr:	case jit_code_subcr:
	case jit_code_mulr:	case jit_code_divr:	case jit_code_divr_u:
	case jit_code_remr:	case jit_code_remr_u:	case jit_code_andr:
	case jit_code_orr:	case jit_code_xorr:	case jit_code_lshr:
	case jit_code_rshr:	case jit_code_rshr_u:	case jit_code_ltr:
	case jit_code_ltr_u:	case jit_code_ler:	case jit_code_ler_u:
	case jit_code_eqr:	case jit_code_ger:	case jit_code_ger_u:
	case jit_code_gtr:	case jit_code_gtr_u:	case jit_code_ner:
	case jit_code_ldxr_c:	case jit_code_ldxr_uc:	case jit_code_ldxr_s:
	case jit_code_ldxr_us:	case jit_code_ldxr_i:	case jit_code_ldxr_ui:
	case jit_code_ldxr_l:	case jit_code_addr_f:	case jit_code_subr_f:
	case jit_code_mulr_f:	case jit_code_divr_f:	case jit_code_ltr_f:
	case jit_code_ler_f:	case jit_code_eqr_f:	case jit_code_ger_f:
	case jit_code_gtr_f:	case jit_code_ner_f:	case jit_code_unltr_f:
	case jit_code_unler_f:	case jit_code_uneqr_f:	case jit_code_unger_f:
	case jit_code_ungtr_f:	case jit_code_ltgtr_f:	case jit_code_ordr_f:
	case jit_code_unordr_f:	case jit_code_ldxr_f:	case jit_code_addr_d:
	case jit_code_subr_d:	case jit_code_mulr_d:	case jit_code_divr_d:
	case jit_code_ltr_d:	case jit_code_ler_d:	case jit_code_eqr_d:
	case jit_code_ger_d:	case jit_code_gtr_d:	case jit_code_ner_d:
	case jit_code_unltr_d:	case jit_code_unler_d:	case jit_code_uneqr_d:
	case jit_code_unger_d:	case jit_code_ungtr_d:	case jit_code_ltgtr_d:
	case jit_code_ordr_d:	case jit_code_unordr_d:	case jit_code_ldxr_d:
	case jit_code_movr_ww_d:
	    mask = jit_cc_a0_reg|jit_cc_a0_chg|jit_cc_a1_reg|jit_cc_a2_reg;
	    break;
	case jit_code_qmulr:	case jit_code_qmulr_u:
	case jit_code_qdivr:	case jit_code_qdivr_u:
	    mask = jit_cc_a0_reg|jit_cc_a0_rlh|jit_cc_a0_chg|
		   jit_cc_a1_reg|jit_cc_a2_reg;
	    break;
	case jit_code_sti_c:	case jit_code_sti_s:	case jit_code_sti_i:
	case jit_code_sti_l:	case jit_code_sti_f:	case jit_code_sti_d:
	    mask = jit_cc_a0_int|jit_cc_a1_reg;
	    break;
	case jit_code_blti:	case jit_code_blti_u:	case jit_code_blei:
	case jit_code_blei_u:	case jit_code_beqi:	case jit_code_bgei:
	case jit_code_bgei_u:	case jit_code_bgti:	case jit_code_bgti_u:
	case jit_code_bnei:	case jit_code_bmsi:	case jit_code_bmci:
	    mask = jit_cc_a0_jmp|jit_cc_a1_reg|jit_cc_a2_int;
	    break;
	case jit_code_blti_f:	case jit_code_blei_f:	case jit_code_beqi_f:
	case jit_code_bgei_f:	case jit_code_bgti_f:	case jit_code_bnei_f:
	case jit_code_bunlti_f:	case jit_code_bunlei_f:	case jit_code_buneqi_f:
	case jit_code_bungei_f:	case jit_code_bungti_f:	case jit_code_bltgti_f:
	case jit_code_bordi_f:	case jit_code_bunordi_f:
	    mask = jit_cc_a0_jmp|jit_cc_a1_reg|jit_cc_a2_flt;
	    break;
	case jit_code_blti_d:	case jit_code_blei_d:	case jit_code_beqi_d:
	case jit_code_bgei_d:	case jit_code_bgti_d:	case jit_code_bnei_d:
	case jit_code_bunlti_d:	case jit_code_bunlei_d:	case jit_code_buneqi_d:
	case jit_code_bungei_d:	case jit_code_bungti_d:	case jit_code_bltgti_d:
	case jit_code_bordi_d:	case jit_code_bunordi_d:
	    mask = jit_cc_a0_jmp|jit_cc_a1_reg|jit_cc_a2_dbl;
	    break;
	case jit_code_str_c:	case jit_code_str_s:	case jit_code_str_i:
	case jit_code_str_l:	case jit_code_str_f:	case jit_code_str_d:
	    mask = jit_cc_a0_reg|jit_cc_a1_reg;
	    break;
	case jit_code_stxi_c:	case jit_code_stxi_s:	case jit_code_stxi_i:
	case jit_code_stxi_l:	case jit_code_stxi_f:	case jit_code_stxi_d:
	    mask = jit_cc_a0_int|jit_cc_a1_reg|jit_cc_a2_reg;
	    break;
	case jit_code_bltr:	case jit_code_bltr_u:	case jit_code_bler:
	case jit_code_bler_u:	case jit_code_beqr:	case jit_code_bger:
	case jit_code_bger_u:	case jit_code_bgtr:	case jit_code_bgtr_u:
	case jit_code_bner:	case jit_code_bmsr:	case jit_code_bmcr:
	case jit_code_bltr_f:	case jit_code_bler_f:	case jit_code_beqr_f:
	case jit_code_bger_f:	case jit_code_bgtr_f:	case jit_code_bner_f:
	case jit_code_bunltr_f:	case jit_code_bunler_f:	case jit_code_buneqr_f:
	case jit_code_bunger_f:	case jit_code_bungtr_f:	case jit_code_bltgtr_f:
	case jit_code_bordr_f:	case jit_code_bunordr_f:case jit_code_bltr_d:
	case jit_code_bler_d:	case jit_code_beqr_d:	case jit_code_bger_d:
	case jit_code_bgtr_d:	case jit_code_bner_d:	case jit_code_bunltr_d:
	case jit_code_bunler_d:	case jit_code_buneqr_d:	case jit_code_bunger_d:
	case jit_code_bungtr_d:	case jit_code_bltgtr_d:	case jit_code_bordr_d:
	case jit_code_bunordr_d:
	    mask = jit_cc_a0_jmp|jit_cc_a1_reg|jit_cc_a2_reg;
	    break;
	case jit_code_boaddi:	case jit_code_boaddi_u:	case jit_code_bxaddi:
	case jit_code_bxaddi_u:	case jit_code_bosubi:	case jit_code_bosubi_u:
	case jit_code_bxsubi:	case jit_code_bxsubi_u:
	    mask = jit_cc_a0_jmp|jit_cc_a1_reg|jit_cc_a1_chg|jit_cc_a2_int;
	    break;
	case jit_code_stxr_c:	case jit_code_stxr_s:	case jit_code_stxr_i:
	case jit_code_stxr_l:	case jit_code_stxr_f:	case jit_code_stxr_d:
	    mask = jit_cc_a0_reg|jit_cc_a1_reg|jit_cc_a2_reg;
	    break;
	case jit_code_boaddr:	case jit_code_boaddr_u:	case jit_code_bxaddr:
	case jit_code_bxaddr_u:	case jit_code_bosubr:	case jit_code_bosubr_u:
	case jit_code_bxsubr:	case jit_code_bxsubr_u:
	    mask = jit_cc_a0_jmp|jit_cc_a1_reg|jit_cc_a1_chg|jit_cc_a2_reg;
	    break;
	default:
	    abort();
    }

    return (mask);
}

void
_jit_patch_abs(jit_state_t *_jit, jit_node_t *instr, jit_pointer_t address)
{
    jit_int32_t		mask;

    if (instr->code == jit_code_movi)
	instr->v.p = address;
    else {
	mask = jit_classify(instr->code);
	assert((mask & (jit_cc_a0_reg|jit_cc_a0_jmp)) == jit_cc_a0_jmp);
	instr->u.p = address;
    }
}

void
_jit_patch_at(jit_state_t *_jit, jit_node_t *instr, jit_node_t *label)
{
    jit_int32_t		mask;

    instr->flag |= jit_flag_node;
    switch (instr->code) {
	case jit_code_movi:
	    assert(label->code == jit_code_label ||
		   label->code == jit_code_data);
	    instr->v.n = label;
	    if (label->code == jit_code_data)
		instr->flag |= jit_flag_data;
	    break;
	case jit_code_jmpi:
	    assert(label->code == jit_code_label ||
		   label->code == jit_code_epilog);
	    instr->u.n = label;
	    break;
	default:
	    mask = jit_classify(instr->code);
	    assert((mask & (jit_cc_a0_reg|jit_cc_a0_jmp)) == jit_cc_a0_jmp);
	    assert(label->code == jit_code_label);
	    instr->u.n = label;
	    break;
    }
    /* link field is used as list of nodes associated with a given label */
    instr->link = label->link;
    label->link = instr;
}

void
_jit_optimize(jit_state_t *_jit)
{
    jit_uint8_t		*ptr;
    jit_bool_t		 jump;
    jit_int32_t		 mask;
    jit_node_t		*node;
    jit_block_t		*block;
    jit_word_t		 offset;

    _jit->function = NULL;

    thread_jumps();
    sequential_labels();

    /* create initial mapping of live register values
     * at the start of a basic block */
    for (offset = 0; offset < _jit->blocks.offset; offset++) {
	block = _jit->blocks.ptr + offset;
	if (!block->label)
	    continue;
	if (block->label->code != jit_code_epilog)
	    jit_setup(block);
    }
    /* call jit_update resolving undefined values in reverse
     * order so that sequential code would find most data already
     * resolved when reaching the start of a new basic block */
    for (offset = _jit->blocks.offset - 1; offset >= 0; offset--) {
	block = _jit->blocks.ptr + offset;
	if (!block->label)
	    continue;
	if (block->label->code != jit_code_epilog) {
	    jit_regset_set(_jit->regmask, block->regmask);
	    jit_update(block->label->next, &block->reglive, &_jit->regmask);
	}
    }

    patch_registers();
    simplify();

    /* figure out labels that are only reached with a jump
     * and is required to do a simple redundant_store removal
     * on jit_beqi below */
    jump = 1;
    for (node = _jit->head; node; node = node->next) {
	switch (node->code) {
	    case jit_code_label:
		if (!jump)
		    node->flag |= jit_flag_head;
		break;
	    case jit_code_jmpi:		case jit_code_jmpr:
	    case jit_code_epilog:
		jump = 1;
		break;
	    case jit_code_data:		case jit_code_note:
		break;
	    default:
		jump = 0;
		break;
	}
    }

    for (node = _jit->head; node; node = node->next) {
	switch (node->code) {
	    case jit_code_prolog:
		_jit->function = _jit->functions.ptr + node->w.w;
		break;
	    case jit_code_epilog:
		_jit->function = NULL;
		break;
	    case jit_code_beqi:
		redundant_store(node, 1);
		break;
	    case jit_code_bnei:
		redundant_store(node, 0);
		break;
	    default:
		mask = jit_classify(node->code);
#if JIT_HASH_CONSTS
		if (mask & jit_cc_a1_flt) {
		    node->v.p = jit_data(&node->v.f, sizeof(jit_float32_t), 4);
		    node->flag |= jit_flag_node | jit_flag_data;
		}
		else if (mask & jit_cc_a1_dbl) {
		    node->v.p = jit_data(&node->v.d, sizeof(jit_float64_t), 8);
		    node->flag |= jit_flag_node | jit_flag_data;
		}
		else if (mask & jit_cc_a2_flt) {
		    node->w.p = jit_data(&node->w.f, sizeof(jit_float32_t), 4);
		    node->flag |= jit_flag_node | jit_flag_data;
		}
		else if (mask & jit_cc_a2_dbl) {
		    node->w.p = jit_data(&node->w.d, sizeof(jit_float64_t), 8);
		    node->flag |= jit_flag_node | jit_flag_data;
		}
#endif
		if (_jit->function) {
		    if ((mask & (jit_cc_a0_reg|jit_cc_a0_chg)) ==
			(jit_cc_a0_reg|jit_cc_a0_chg))
			jit_regset_setbit(_jit->function->regset,
					  jit_regno(node->u.w));
		    if ((mask & (jit_cc_a1_reg|jit_cc_a1_chg)) ==
			(jit_cc_a1_reg|jit_cc_a1_chg))
			jit_regset_setbit(_jit->function->regset,
					  jit_regno(node->v.w));
		}
		break;
	}
    }

    /* ensure it is aligned */
    _jit->data.offset = (_jit->data.offset + 7) & -8;

    /* create read only data buffer */
    _jit->data.length = (_jit->data.offset +
			 /* reserve space for annotations */
			 _jit->note.size + 4095) & -4096;
    ptr = mmap(NULL, _jit->data.length,
	       PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANON, -1, 0);
    assert(ptr != MAP_FAILED);
    memcpy(ptr, _jit->data.ptr, _jit->data.offset);
    free(_jit->data.ptr);
    _jit->data.ptr = ptr;

    /* to be filled with note contents once offsets are known */
    _jit->note.base = ptr + _jit->data.offset;
    memset(_jit->note.base, 0, _jit->data.length - _jit->data.offset);

    for (offset = 0; offset < _jit->data.size; offset++) {
	for (node = _jit->data.table[offset]; node; node = node->next) {
	    node->flag |= jit_flag_patch;
	    node->u.w = (jit_word_t)(_jit->data.ptr + node->u.w);
	}
    }
}

void
_jit_reglive(jit_state_t *_jit, jit_node_t *node)
{
    jit_int32_t		 spec;
    jit_int32_t		 value;
    jit_block_t		*block;

    switch (node->code) {
	case jit_code_label:	case jit_code_prolog:	case jit_code_epilog:
	    block = _jit->blocks.ptr + node->v.w;
	    jit_regset_set(_jit->reglive, block->reglive);
	    break;
	case jit_code_callr:
	    value = jit_regno(node->u.w);
	    if (!(node->u.w & jit_regno_patch)) {
		jit_regset_setbit(_jit->reglive, value);
	    }
	case jit_code_calli:
	    for (value = 0; value < _jit->reglen; value++) {
		spec = jit_class(_rvs[value].spec);
		if ((spec & jit_class_arg) && jit_regarg_p(node, value))
		    jit_regset_setbit(_jit->reglive, value);
		else if (!(spec & jit_class_sav))
		    jit_regset_clrbit(_jit->reglive, value);
	    }
#if defined(JIT_RET)
	    /* Explicitly set return registers as live, because retval
	     * should be free to not create a note, and/or user not
	     * call jit_retval (but not a good idea to expect JIT_R0
	     * to match JIT_RET) */
	    jit_regset_setbit(_jit->reglive, JIT_RET);
#  if __arm__
	    /* FIXME need a better logic (and r2-r3 may contain results) */
	    jit_regset_setbit(_jit->reglive, _R1);
#  endif
#endif
#if defined(JIT_FRET)
	    jit_regset_setbit(_jit->reglive, JIT_FRET);
#endif
	    break;
	default:
	    value = jit_classify(node->code);
	    if (value & jit_cc_a0_reg) {
		if (value & jit_cc_a0_rlh) {
		    if (!(node->u.q.l & jit_regno_patch)) {
			if (value & jit_cc_a0_chg) {
			    jit_regset_clrbit(_jit->reglive, node->u.q.l);
			    jit_regset_setbit(_jit->regmask, node->u.q.l);
			}
			else
			    jit_regset_setbit(_jit->reglive, node->u.q.l);
		    }
		    if (!(node->u.q.h & jit_regno_patch)) {
			if (value & jit_cc_a0_chg) {
			    jit_regset_clrbit(_jit->reglive, node->u.q.h);
			    jit_regset_setbit(_jit->regmask, node->u.q.h);
			}
			else
			    jit_regset_setbit(_jit->reglive, node->u.q.h);
		    }
		}
		else {
		    if (!(node->u.w & jit_regno_patch)) {
			if (value & jit_cc_a0_chg) {
			    jit_regset_clrbit(_jit->reglive, node->u.w);
			    jit_regset_setbit(_jit->regmask, node->u.w);
			}
			else
			    jit_regset_setbit(_jit->reglive, node->u.w);
		    }
		}
	    }
	    if ((value & jit_cc_a1_reg) && !(node->v.w & jit_regno_patch)) {
		if (value & jit_cc_a1_chg) {
		    jit_regset_clrbit(_jit->reglive, node->v.w);
		    jit_regset_setbit(_jit->regmask, node->v.w);
		}
		else
		    jit_regset_setbit(_jit->reglive, node->v.w);
	    }
	    if ((value & jit_cc_a2_reg) && !(node->w.w & jit_regno_patch))
		jit_regset_setbit(_jit->reglive, node->w.w);
	    if (jit_regset_set_p(_jit->regmask)) {
		mpz_set_ui(_jit->blockmask, 0);
		jit_update(node->next, &_jit->reglive, &_jit->regmask);
		if (jit_regset_set_p(_jit->regmask)) {
		    /* any unresolved live state is considered as live */
		    jit_regset_ior(_jit->reglive, _jit->reglive, _jit->regmask);
		    jit_regset_set_ui(_jit->regmask, 0);
		}
	    }
	    break;
    }
}

void
_jit_regarg_set(jit_state_t *_jit, jit_node_t *node, jit_int32_t value)
{
    if (value & jit_cc_a0_reg) {
	if (value & jit_cc_a0_rlh) {
	    jit_regset_setbit(_jit->regarg, jit_regno(node->u.q.l));
	    jit_regset_setbit(_jit->regarg, jit_regno(node->u.q.h));
	}
	else
	    jit_regset_setbit(_jit->regarg, jit_regno(node->u.w));
    }
    if (value & jit_cc_a1_reg)
	jit_regset_setbit(_jit->regarg, jit_regno(node->v.w));
    if (value & jit_cc_a2_reg)
	jit_regset_setbit(_jit->regarg, jit_regno(node->w.w));
}

void
_jit_regarg_clr(jit_state_t *_jit, jit_node_t *node, jit_int32_t value)
{
    if (value & jit_cc_a0_reg) {
	if (value & jit_cc_a0_rlh) {
	    jit_regset_clrbit(_jit->regarg, jit_regno(node->u.q.l));
	    jit_regset_clrbit(_jit->regarg, jit_regno(node->u.q.h));
	}
	else
	    jit_regset_clrbit(_jit->regarg, jit_regno(node->u.w));
    }
    if (value & jit_cc_a1_reg)
	jit_regset_clrbit(_jit->regarg, jit_regno(node->v.w));
    if (value & jit_cc_a2_reg)
	jit_regset_clrbit(_jit->regarg, jit_regno(node->w.w));
}

jit_pointer_t
_jit_emit(jit_state_t *_jit)
{
    jit_pointer_t	 code;
    jit_node_t		*node;
    jit_int32_t		 mult;
    size_t		 length;
    int			 result;

    if (_jit->function)
	jit_epilog();
    jit_optimize();

    /* Heuristic to guess code buffer size */
    mult = 4;

    _jit->emit = 1;

    _jit->code.length = _jit->pool.length * 1024 * mult;

    _jit->code.ptr = mmap(NULL, _jit->code.length,
			  PROT_EXEC | PROT_READ | PROT_WRITE,
			  MAP_PRIVATE | MAP_ANON, -1, 0);
    assert(_jit->code.ptr != MAP_FAILED);
    _jit->code.end = _jit->code.ptr + _jit->code.length - 64;
    _jit->pc.uc = _jit->code.ptr;

    for (;;) {
	if ((code = emit_code()) == NULL) {
	    for (node = _jit->head; node; node = node->next) {
		if (node->code == jit_code_label && node->link)
		    node->flag &= ~jit_flag_patch;
	    }
	    ++mult;
	    length = _jit->pool.length * 1024 * mult;

#if !HAVE_MREMAP
	    munmap(_jit->code.ptr, _jit->code.length);
#endif

#if HAVE_MREMAP
	    _jit->code.ptr = mremap(_jit->code.ptr, _jit->code.length,
				    length, MREMAP_MAYMOVE, NULL);
#else
	    _jit->code.ptr = mmap(NULL, length,
				  PROT_EXEC | PROT_READ | PROT_WRITE,
				  MAP_PRIVATE | MAP_ANON, -1, 0);
#endif

	    assert(_jit->code.ptr != MAP_FAILED);
	    _jit->code.length = length;
	    _jit->code.end = _jit->code.ptr + _jit->code.length - 64;
	    _jit->pc.uc = _jit->code.ptr;
	    _jit->patches.offset = 0;
	}
	else
	    break;
    }

    _jit->done = 1;
    jit_annotate();

    result = mprotect(_jit->data.ptr, _jit->data.length, PROT_READ);
    assert(result == 0);
    result = mprotect(_jit->code.ptr, _jit->code.length, PROT_READ | PROT_EXEC);
    assert(result == 0);

    return (code);
}

/*   Compute initial reglive and regmask set values of a basic block.
 * reglive is the set of known live registers
 * regmask is the set of registers not referenced in the block
 */
static void
_jit_setup(jit_state_t *_jit, jit_block_t *block)
{
#define reglive			block->reglive
#define regmask			block->regmask
    jit_node_t		*node;
    jit_bool_t		 live;
    jit_bool_t		 jump;
    unsigned long	 value;

    jump = 0;
    jit_regset_set_ui(regmask, (1LL << _jit->reglen) - 1);
    for (node = block->label->next; node; node = node->next) {
	switch (node->code) {
	    case jit_code_label:	case jit_code_prolog:
	    case jit_code_epilog:
		return;
	    default:
		value = jit_classify(node->code);
		if (value & jit_cc_a0_reg) {
		    live = !(value & jit_cc_a0_chg);
		    if (value & jit_cc_a0_rlh) {
			if (!(node->u.q.l & jit_regno_patch) &&
			    jit_regset_tstbit(regmask, node->u.q.l)) {
			    if (live || !jump)
				jit_regset_clrbit(regmask, node->u.q.l);
			    if (live)
				jit_regset_setbit(reglive, node->u.q.l);
			}
			if (!(node->u.q.h & jit_regno_patch) &&
			    jit_regset_tstbit(regmask, node->u.q.h)) {
			    if (live || !jump)
				jit_regset_clrbit(regmask, node->u.q.h);
			    if (live)
				jit_regset_setbit(reglive, node->u.q.h);
			}
		    }
		    else {
			if (!(node->u.w & jit_regno_patch) &&
			    jit_regset_tstbit(regmask, node->u.w)) {
			    if (live || !jump)
				jit_regset_clrbit(regmask, node->u.w);
			    if (live)
				jit_regset_setbit(reglive, node->u.w);
			}
		    }
		}
		if ((value & jit_cc_a1_reg) &&
		    !(node->v.w & jit_regno_patch) &&
		    jit_regset_tstbit(regmask, node->v.w)) {
		    live = !(value & jit_cc_a1_chg);
		    if (live || !jump)
			jit_regset_clrbit(regmask, node->v.w);
		    if (live)
			jit_regset_setbit(reglive, node->v.w);
		}
		if ((value & jit_cc_a2_reg) &&
		    !(node->w.w & jit_regno_patch) &&
		    jit_regset_tstbit(regmask, node->w.w)) {
		    jit_regset_clrbit(regmask, node->w.w);
		    jit_regset_setbit(reglive, node->w.w);
		}
		if (value & jit_cc_a0_jmp)
		    jump = 1;
		break;
	}
    }
#undef regmask
#undef reglive
}

/*   Remove bit of mask argument based on instructions arguments up to end
 * of code or finding a basic block boundary, if a value is used as argument,
 * also set the live bit to know value cannot be clobbered; if value is
 * modified, just remove it from the mask as if it not already in the live
 * bitmask, then the value is dead.
 */
static void
_jit_update(jit_state_t *_jit, jit_node_t *node,
	    jit_regset_t *live, jit_regset_t *mask)
{
    jit_int32_t		 spec;
    jit_regset_t	 ztmp;
    jit_regset_t	 zmask;
    unsigned long	 value;
    jit_block_t		*block;
    jit_node_t		*label;

    for (; node; node = node->next) {
    restart:
	if (jit_regset_set_p(*mask) == 0)
	    break;
	switch (node->code) {
	    case jit_code_label:
		block = _jit->blocks.ptr + node->v.w;
		if (mpz_tstbit(_jit->blockmask, node->v.w))
		    return;
		mpz_setbit(_jit->blockmask, node->v.w);
		jit_regset_and(ztmp, *mask, block->reglive);
		if (jit_regset_set_p(ztmp)) {
		    jit_regset_ior(*live, *live, ztmp);
		    jit_regset_com(ztmp, ztmp);
		    jit_regset_and(*mask, *mask, ztmp);
		}
		break;
	    case jit_code_prolog:	case jit_code_epilog:
		jit_regset_set_ui(*mask, 0);
		return;
	    case jit_code_callr:
		value = jit_regno(node->u.w);
		if (!(node->u.w & jit_regno_patch)) {
		    if (jit_regset_tstbit(*mask, value)) {
			jit_regset_clrbit(*mask, value);
			jit_regset_setbit(*live, value);
		    }
		}
	    case jit_code_calli:
#if defined(JIT_RET)
		if (jit_regset_tstbit(*mask, JIT_RET)) {
		    jit_regset_setbit(_jit->reglive, JIT_RET);
		    jit_regset_clrbit(*mask, JIT_RET);
		}
#  if __arm__
		if (jit_regset_tstbit(*mask, _R1)) {
		    jit_regset_setbit(_jit->reglive, _R1);
		    jit_regset_clrbit(*mask, _R1);
		}
#  endif
#endif
#if defined(JIT_FRET)
		if (jit_regset_tstbit(*mask, JIT_FRET)) {
		    jit_regset_setbit(_jit->reglive, JIT_FRET);
		    jit_regset_clrbit(*mask, JIT_FRET);
		}
#endif
		for (value = 0; value < _jit->reglen; ++value) {
		    value = jit_regset_scan1(*mask, value);
		    if (value >= _jit->reglen)
			break;
		    spec = jit_class(_rvs[value].spec);
		    if (!(spec & jit_class_sav))
			jit_regset_clrbit(*mask, value);
		    if ((spec & jit_class_arg) && jit_regarg_p(node, value))
			jit_regset_setbit(*live, value);
		}
		break;
	    default:
		value = jit_classify(node->code);
		if (value & jit_cc_a2_reg) {
		    if (!(node->w.w & jit_regno_patch)) {
			if (jit_regset_tstbit(*mask, node->w.w)) {
			    jit_regset_clrbit(*mask, node->w.w);
			    jit_regset_setbit(*live, node->w.w);
			}
		    }
		}
		if (value & jit_cc_a1_reg) {
		    if (!(node->v.w & jit_regno_patch)) {
			if (jit_regset_tstbit(*mask, node->v.w)) {
			    jit_regset_clrbit(*mask, node->v.w);
			    if (!(value & jit_cc_a1_chg))
				jit_regset_setbit(*live, node->v.w);
			}
		    }
		}
		if (value & jit_cc_a0_reg) {
		    if (value & jit_cc_a0_rlh) {
			if (!(node->u.q.l & jit_regno_patch)) {
			    if (jit_regset_tstbit(*mask, node->u.q.l)) {
				jit_regset_clrbit(*mask, node->u.q.l);
				if (!(value & jit_cc_a0_chg))
				    jit_regset_setbit(*live, node->u.q.l);
			    }
			}
			if (!(node->u.q.h & jit_regno_patch)) {
			    if (jit_regset_tstbit(*mask, node->u.q.h)) {
				jit_regset_clrbit(*mask, node->u.q.h);
				if (!(value & jit_cc_a0_chg))
				    jit_regset_setbit(*live, node->u.q.h);
			    }
			}
		    }
		    else {
			if (!(node->u.w & jit_regno_patch)) {
			    if (jit_regset_tstbit(*mask, node->u.w)) {
				jit_regset_clrbit(*mask, node->u.w);
				if (!(value & jit_cc_a0_chg))
				    jit_regset_setbit(*live, node->u.w);
			    }
			}
		    }
		}
		if (value & jit_cc_a0_jmp) {
		    if (node->flag & jit_flag_node) {
			label = node->u.n;
			if (node->code == jit_code_jmpi) {
			    node = label;
			    goto restart;
			}
			block = _jit->blocks.ptr + label->v.w;
			if (mpz_tstbit(_jit->blockmask, label->v.w))
			    continue;
			mpz_setbit(_jit->blockmask, label->v.w);
			jit_regset_and(ztmp, *mask, block->reglive);
			if (jit_regset_set_p(ztmp)) {
			    jit_regset_ior(*live, *live, ztmp);
			    jit_regset_com(ztmp, ztmp);
			    jit_regset_and(*mask, *mask, ztmp);
			}
			if (jit_regset_set_p(*mask) == 0)
			    return;
			/* restore mask if branch is conditional */
			zmask = *mask;
			jit_update(block->label->next, live, &zmask);
			jit_regset_xor(ztmp, zmask, *mask);
			/* remove known live registers from mask */
			if (jit_regset_set_p(ztmp)) {
			    jit_regset_and(ztmp, ztmp, *live);
			    jit_regset_com(ztmp, ztmp);
			    jit_regset_and(*mask, *mask, ztmp);
			}
			continue;
		    }
		    /* assume value is live due to jump to unknown location */
		    jit_regset_ior(*live, *live, *mask);
		    jit_regset_set_ui(*mask, 0);
		    return;
		}
		break;
	}
    }
}

static void
_thread_jumps(jit_state_t *_jit)
{
    jit_node_t		*prev;
    jit_node_t		*node;
    jit_node_t		*next;
    jit_int32_t		 mask;

    for (prev = node = _jit->head; node;) {
	next = node->next;
	switch (node->code) {
	    case jit_code_jmpi:
		if (redundant_jump(prev, node)) {
		    node = prev;
		    continue;
		}
		if (shortcut_jump(prev, node))
		    continue;
		break;
	    case jit_code_jmpr:
	    case jit_code_callr:	case jit_code_calli:
		/* non optimizable jump like code */
		break;
	    default:
		mask = jit_classify(node->code);
		if (mask & jit_cc_a0_jmp) {
		    if (reverse_jump(prev, node) ||
			shortcut_jump(prev, node))
			continue;
		}
		break;
	}
	prev = node;
	node = next;
    }
}

static void
_sequential_labels(jit_state_t *_jit)
{
    jit_node_t		*jump;
    jit_node_t		*link;
    jit_node_t		*prev;
    jit_node_t		*next;
    jit_node_t		*node;

    for (prev = node = _jit->head; node; node = next) {
	next = node->next;
	if (node->code == jit_code_label) {
	    if (!node->flag) {
		if (!node->link) {
		    del_label(prev, node);
		    continue;
		}
		if (prev != node && prev->code == jit_code_label) {
		    if ((jump = node->link)) {
			for (; jump; jump = link) {
			    link = jump->link;
			    jump->u.n = prev;
			    jump->link = prev->link;
			    prev->link = jump;
			}
			node->link = NULL;
		    }
		    del_label(prev, node);
		    continue;
		}
	    }
	    if (next && next->code == jit_code_label && !next->flag) {
		if ((jump = next->link)) {
		    for (; jump; jump = link) {
			link = jump->link;
			jump->u.n = node;
			jump->link = node->link;
			node->link = jump;
		    }
		    next->link = NULL;
		}
		del_label(node, next);
		next = node->next;
		continue;
	    }
	}
	prev = node;
    }
}

static jit_bool_t
_shortcut_jump(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node)
{
    jit_bool_t		 cond;
    jit_node_t		*jump;
    jit_node_t		*next;
    jit_node_t		*temp;

    if (!(node->flag & jit_flag_node))
	return (0);
    assert(node->code != jit_code_jmpr);
    cond = node->code != jit_code_jmpi;
    jump = node->u.n;
    for (next = jump->next; next; next = next->next) {
	switch (next->code) {
	    case jit_code_jmpi:
		if (jump->link == node)
		    jump->link = node->link;
		else {
		    for (temp = jump->link;
			 temp->link != node;
			 temp = temp->link)
			assert(temp != NULL);
		    temp->link = node->link;
		}
		jump = next->u.n;
		node->u.n = jump;
		node->link = jump->link;
		jump->link = node;
		return (1);
	    case jit_code_jmpr:
		if (cond)
		    return (0);
		node->code = jit_code_jmpr;
		node->u.w = next->u.w;
		node->link = NULL;
		node->flag &= ~jit_flag_node;
		return (1);
	    case jit_code_note:		case jit_code_label:
		break;
	    default:
		return (0);
	}
    }
    return (0);
}

static jit_bool_t
_redundant_jump(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node)
{
    jit_node_t		*local_prev;
    jit_node_t		*local_next;

    if (!(node->flag & jit_flag_node))
	return (0);
    for (local_prev = node, local_next = node->next;
	 local_next;
	 local_prev = local_next, local_next = local_next->next) {

	switch (local_next->code) {
	    case jit_code_label:	case jit_code_epilog:
		if (node->u.n == local_next) {
		    if (local_next->link == node)
			local_next->link = node->link;
		    else {
			for (local_prev = local_next->link;
			     local_prev->link != node;
			     local_prev = local_prev->link)
			    assert(local_prev != NULL);
			local_prev->link = node->link;
		    }
		    del_node(prev, node);
		    return (1);
		}
		break;
	    case jit_code_note:
		break;
	    default:
		return (0);
	}
    }
    return (0);
}

static jit_code_t
reverse_jump_code(jit_code_t code)
{
    switch (code) {
	case jit_code_bltr:	return (jit_code_bger);
	case jit_code_blti:	return (jit_code_bgei);
	case jit_code_bltr_u:	return (jit_code_bger_u);
	case jit_code_blti_u:	return (jit_code_bgei_u);
	case jit_code_bler:	return (jit_code_bgtr);
	case jit_code_blei:	return (jit_code_bgti);
	case jit_code_bler_u:	return (jit_code_bgtr_u);
	case jit_code_blei_u:	return (jit_code_bgti_u);
	case jit_code_beqr:	return (jit_code_bner);
	case jit_code_beqi:	return (jit_code_bnei);
	case jit_code_bger:	return (jit_code_bltr);
	case jit_code_bgei:	return (jit_code_blti);
	case jit_code_bger_u:	return (jit_code_bltr_u);
	case jit_code_bgei_u:	return (jit_code_blti_u);
	case jit_code_bgtr:	return (jit_code_bler);
	case jit_code_bgti:	return (jit_code_blei);
	case jit_code_bgtr_u:	return (jit_code_bler_u);
	case jit_code_bgti_u:	return (jit_code_blei_u);
	case jit_code_bner:	return (jit_code_beqr);
	case jit_code_bnei:	return (jit_code_beqi);
	case jit_code_bmsr:	return (jit_code_bmcr);
	case jit_code_bmsi:	return (jit_code_bmci);
	case jit_code_bmcr:	return (jit_code_bmsr);
	case jit_code_bmci:	return (jit_code_bmsi);
	case jit_code_bltr_f:	return (jit_code_bunger_f);
	case jit_code_blti_f:	return (jit_code_bungei_f);
	case jit_code_bler_f:	return (jit_code_bungtr_f);
	case jit_code_blei_f:	return (jit_code_bungti_f);

	case jit_code_beqr_f:	return (jit_code_bner_f);
	case jit_code_beqi_f:	return (jit_code_bnei_f);

	case jit_code_bger_f:	return (jit_code_bunltr_f);
	case jit_code_bgei_f:	return (jit_code_bunlti_f);
	case jit_code_bgtr_f:	return (jit_code_bunler_f);
	case jit_code_bgti_f:	return (jit_code_bunlei_f);

	case jit_code_bner_f:	return (jit_code_beqr_f);
	case jit_code_bnei_f:	return (jit_code_beqr_f);

	case jit_code_bunltr_f:	return (jit_code_bger_f);
	case jit_code_bunlti_f:	return (jit_code_bgei_f);
	case jit_code_bunler_f:	return (jit_code_bgtr_f);
	case jit_code_bunlei_f:	return (jit_code_bgti_f);

	case jit_code_buneqr_f:	return (jit_code_bltgtr_f);
	case jit_code_buneqi_f:	return (jit_code_bltgti_f);

	case jit_code_bunger_f:	return (jit_code_bltr_f);
	case jit_code_bungei_f:	return (jit_code_blti_f);
	case jit_code_bungtr_f:	return (jit_code_bler_f);
	case jit_code_bungti_f:	return (jit_code_blei_f);

	case jit_code_bltgtr_f:	return (jit_code_buneqr_f);
	case jit_code_bltgti_f:	return (jit_code_buneqi_f);

	case jit_code_bordr_f:	return (jit_code_bunordr_f);
	case jit_code_bordi_f:	return (jit_code_bunordi_f);
	case jit_code_bunordr_f:return (jit_code_bordr_f);
	case jit_code_bunordi_f:return (jit_code_bordi_f);
	case jit_code_bltr_d:	return (jit_code_bunger_d);
	case jit_code_blti_d:	return (jit_code_bungei_d);
	case jit_code_bler_d:	return (jit_code_bungtr_d);
	case jit_code_blei_d:	return (jit_code_bungti_d);

	case jit_code_beqr_d:	return (jit_code_bner_d);
	case jit_code_beqi_d:	return (jit_code_bnei_d);

	case jit_code_bger_d:	return (jit_code_bunltr_d);
	case jit_code_bgei_d:	return (jit_code_bunlti_d);
	case jit_code_bgtr_d:	return (jit_code_bunler_d);
	case jit_code_bgti_d:	return (jit_code_bunlei_d);

	case jit_code_bner_d:	return (jit_code_beqr_d);
	case jit_code_bnei_d:	return (jit_code_beqi_d);

	case jit_code_bunltr_d:	return (jit_code_bger_d);
	case jit_code_bunlti_d:	return (jit_code_bgei_d);
	case jit_code_bunler_d:	return (jit_code_bgtr_d);
	case jit_code_bunlei_d:	return (jit_code_bgti_d);

	case jit_code_buneqr_d:	return (jit_code_bltgtr_d);
	case jit_code_buneqi_d:	return (jit_code_bltgti_d);

	case jit_code_bunger_d:	return (jit_code_bltr_d);
	case jit_code_bungei_d:	return (jit_code_blti_d);
	case jit_code_bungtr_d:	return (jit_code_bler_d);
	case jit_code_bungti_d:	return (jit_code_blei_d);

	case jit_code_bltgtr_d:	return (jit_code_buneqr_d);
	case jit_code_bltgti_d:	return (jit_code_buneqi_d);

	case jit_code_bordr_d:	return (jit_code_bunordr_d);
	case jit_code_bordi_d:	return (jit_code_bunordi_d);
	case jit_code_bunordr_d:return (jit_code_bordr_d);
	case jit_code_bunordi_d:return (jit_code_bordi_d);
	case jit_code_boaddr:	return (jit_code_bxaddr);
	case jit_code_boaddi:	return (jit_code_bxaddi);
	case jit_code_boaddr_u:	return (jit_code_bxaddr_u);
	case jit_code_boaddi_u:	return (jit_code_bxaddi_u);
	case jit_code_bxaddr:	return (jit_code_boaddr);
	case jit_code_bxaddi:	return (jit_code_boaddi);
	case jit_code_bxaddr_u:	return (jit_code_boaddr_u);
	case jit_code_bxaddi_u:	return (jit_code_boaddi_u);
	case jit_code_bosubr:	return (jit_code_bxsubr);
	case jit_code_bosubi:	return (jit_code_bxsubi);
	case jit_code_bosubr_u:	return (jit_code_bxsubr_u);
	case jit_code_bosubi_u:	return (jit_code_bxsubi_u);
	case jit_code_bxsubr:	return (jit_code_bosubr);
	case jit_code_bxsubi:	return (jit_code_bosubi);
	case jit_code_bxsubr_u:	return (jit_code_bosubr_u);
	case jit_code_bxsubi_u:	return (jit_code_bosubi_u);
	default:		abort();	/* invalid jump code */
    }
}

/*
 * change common pattern:
 *	<cond_jump L0> <jump L1> <label L0>
 * into
 *	<reverse_cond_jump L1>
 */
static jit_bool_t
_reverse_jump(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node)
{
    jit_node_t		*local_prev;
    jit_node_t		*local_next;
    jit_node_t		*local_jump;

    if (!(node->flag & jit_flag_node))
	return (0);
    /* =><cond_jump L0> <jump L1> <label L0> */
    local_next = node->next;
    if (local_next->code != jit_code_jmpi)
	return (0);
    /* <cond_jump L0> =><jump L1> <label L0> */

    local_jump = local_next->u.n;
    for (local_prev = local_next, local_next = local_next->next;
	 local_next;
	 local_prev = local_next, local_next = local_next->next) {
	switch (local_next->code) {
	    case jit_code_label:	case jit_code_epilog:
		if (node->u.n == local_next) {
		    if (local_next->link == node)
			local_next->link = node->link;
		    else {
			for (local_prev = local_next->link;
			     local_prev->link != node;
			     local_prev = local_prev->link)
			    assert(local_prev != NULL);
			local_prev->link = node->link;
		    }
		    del_node(node, node->next);
		    node->code = reverse_jump_code(node->code);
		    node->u.n = local_jump;
		    node->link = local_jump->link;
		    local_jump->link = node;
		    return (1);
		}
		break;
	    case jit_code_note:
		break;
	    default:
		return (0);
	}
    }
    return (0);
}

static void
_redundant_store(jit_state_t *_jit, jit_node_t *node, jit_bool_t jump)
{
    jit_node_t		*iter;
    jit_node_t		*prev;
    jit_word_t		 word;
    jit_int32_t		 spec;
    jit_int32_t		 regno;

    if (jump) {
	prev = node->u.n;
	if (prev->code == jit_code_epilog)
	    return;
	assert(prev->code == jit_code_label);
	if ((prev->flag & jit_flag_head) || node->link || prev->link != node)
	    /* multiple sources */
	    return;
	/* if there are sequential labels it will return below */
    }
    else
	prev = node;
    word = node->w.w;
    regno = jit_regno(node->v.w);
    for (iter = prev->next; iter; prev = iter, iter = iter->next) {
	switch (iter->code) {
	    case jit_code_label:	case jit_code_prolog:
	    case jit_code_epilog:
		return;
	    case jit_code_movi:
		if (regno == jit_regno(iter->u.w)) {
		    if (iter->flag || iter->v.w != word)
			return;
		    del_node(prev, iter);
		    iter = prev;
		}
		break;
	    default:
		spec = jit_classify(iter->code);
		if (spec & jit_cc_a0_jmp)
		    return;
		if ((spec & (jit_cc_a0_reg|jit_cc_a0_chg)) ==
		    (jit_cc_a0_reg|jit_cc_a0_chg)) {
		    if (spec & jit_cc_a0_rlh) {
			if (regno == jit_regno(iter->u.q.l) ||
			    regno == jit_regno(iter->u.q.h))
			    return;
		    }
		    else {
			if (regno == jit_regno(iter->u.w))
			    return;
		    }
		}
		if ((spec & (jit_cc_a1_reg|jit_cc_a1_chg)) ==
		    (jit_cc_a1_reg|jit_cc_a1_chg)) {
		    if (regno == jit_regno(iter->v.w))
			return;
		}
		break;
	}
    }
}

static jit_bool_t
_simplify_movr(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node,
	       jit_int32_t kind, jit_int32_t size)
{
    jit_int32_t		 regno;
    jit_int32_t		 right;
    jit_value_t		*value;

    regno = jit_regno(node->u.w);
    right = jit_regno(node->v.w);
    value = _jit->values + regno;
    if ((value->kind == jit_kind_register &&
	 jit_regno(value->base.q.l) == right &&
	 value->base.q.h == _jit->gen[right]) ||
	(value->kind == kind && _jit->values[right].kind == kind &&
	 memcmp(&value->base.w, &_jit->values[right].base.w, size) == 0)) {
	del_node(prev, node);
	return (1);
    }
    if (_jit->values[right].kind == jit_kind_word)
	memcpy(value, _jit->values + right, sizeof(jit_value_t));
    else {
	value->kind = jit_kind_register;
	value->base.q.l = right;
	value->base.q.h = _jit->gen[regno];
    }
    ++_jit->gen[regno];

    return (0);
}

static jit_bool_t
_simplify_movi(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node,
	       jit_int32_t kind, jit_int32_t size)
{
    jit_value_t		*value;
    jit_int32_t		 spec;
    jit_int32_t		 regno;
    jit_int32_t		 offset;

    regno = jit_regno(node->u.w);
    value = _jit->values + regno;
    if (node->flag & jit_flag_node) {
	/* set to undefined if value will be patched */
	value->kind = 0;
	++_jit->gen[regno];
	return (0);
    }
    if (value->kind == kind) {
	if (memcmp(&node->v.w, &value->base.w, size) == 0) {
	    del_node(prev, node);
	    return (1);
	}
	spec = jit_class(_rvs[regno].spec);
	if (kind == jit_kind_word)
	    spec &= jit_class_gpr;
	else
	    spec &= (jit_class_xpr | jit_class_fpr);
	for (offset = 0; offset < _jit->reglen; offset++) {
	    if (_jit->values[offset].kind == kind &&
		memcmp(&node->v.w, &_jit->values[offset].base.w, size) == 0 &&
		(jit_class(_rvs[offset].spec) & spec) == spec) {
		if (kind == jit_kind_word)
		    node->code = jit_code_movr;
		else if (kind == jit_kind_float32)
		    node->code = jit_code_movr_f;
		else
		    node->code = jit_code_movr_d;
		node->v.w = offset;
		memcpy(value, _jit->values + offset, sizeof(jit_value_t));
		++_jit->gen[regno];
		return (0);
	    }
	}
    }
    value->kind = kind;
    memcpy(&value->base.w, &node->v.w, size);
    ++_jit->gen[regno];

    return (0);
}

/* simple/safe redundandy test not checking if another register
 * holds the same value
 */
static jit_bool_t
_simplify_ldxi(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node)
{
    jit_value_t		*value;
    jit_int32_t		 regno;
    jit_int32_t		 right;

    regno = jit_regno(node->u.w);
    right = jit_regno(node->v.w);
    value = _jit->values + regno;
    if (value->kind == jit_kind_code && value->code == node->code &&
	value->base.q.l == right && value->base.q.h == _jit->gen[right] &&
	node->w.w == value->disp.w) {
	del_node(prev, node);
	return (1);
    }
    value->kind = jit_kind_code;
    value->code = node->code;
    value->base.q.l = right;
    value->base.q.h = _jit->gen[right];
    value->disp.w = node->w.w;
    ++_jit->gen[regno];

    return (0);
}

static jit_bool_t
_simplify_stxi(jit_state_t *_jit, jit_node_t *prev, jit_node_t *node)
{
    jit_value_t		*value;
    jit_int32_t		 regno;
    jit_int32_t		 right;
    jit_int32_t		 offset;

    regno = jit_regno(node->w.w);
    right = jit_regno(node->v.w);
    value = _jit->values + regno;

    /* check for redundant store after load */
    if (value->kind == jit_kind_code && value->code == node->code &&
	value->base.q.l == right && value->base.q.h == _jit->gen[right] &&
	node->w.w == value->disp.w) {
	del_node(prev, node);
	return (1);
    }

    /* assume anything can alias, and invalidate tracked values */
    for (offset = 0; offset < _jit->reglen; offset++) {
	if (_jit->values[offset].kind == jit_kind_code) {
	    _jit->values[offset].kind = 0;
	    ++_jit->gen[offset];
	}
    }

    /* no multiple information, so, if set to a constant,
     * prefer to keep that information */
    if (value->kind == 0) {
	value->kind = jit_kind_code;
	switch (node->code) {
	    /* no information about signed/unsigned either */
	    case jit_code_stxi_c:	value->code = jit_code_ldxi_c;	break;
	    case jit_code_stxi_s:	value->code = jit_code_ldxi_s;	break;
	    case jit_code_stxi_i:	value->code = jit_code_ldxi_i;	break;
	    case jit_code_stxi_l:	value->code = jit_code_ldxi_l;	break;
	    case jit_code_stxi_f:	value->code = jit_code_ldxi_f;	break;
	    case jit_code_stxi_d:	value->code = jit_code_ldxi_d;	break;
	    default:		 	abort();
	}
	value->kind = jit_kind_code;
	value->code = node->code;
	value->base.q.l = right;
	value->base.q.h = _jit->gen[right];
	value->disp.w = node->u.w;
    }

    return (0);
}

/* usually there should be only one store in the
 * jit_get_reg/jit_unget_reg, but properly handle
 * multiple ones by moving the save node */
static void
_simplify_spill(jit_state_t *_jit, jit_node_t *node, jit_int32_t regno)
{
    jit_node_t		*save;
    jit_node_t		*temp;

    if ((temp = _jit->spill[regno]) && (save = temp->next) != node) {
	temp->next = save->next;
	save->next = node->next;
	node->next = save;
	_jit->spill[regno] = node;
    }
}

/* checks for simple cases where a register is set more than
 * once to the same value, and is a common pattern of calls
 * to jit_pushargi and jit_pushargr
 */
static void
_simplify(jit_state_t *_jit)
{
    jit_node_t		*prev;
    jit_node_t		*node;
    jit_node_t		*next;
    jit_int32_t		 info;
    jit_int32_t		 regno;
    jit_int32_t		 offset;

    for (prev = NULL, node = _jit->head; node; prev = node, node = next) {
	next = node->next;
	switch (node->code) {
	    case jit_code_label:	case jit_code_prolog:
	    reset:
		memset(_jit->gen, 0, sizeof(jit_int32_t) * _jit->reglen);
		memset(_jit->values, 0, sizeof(jit_value_t) * _jit->reglen);
		break;
	    case jit_code_save:
		_jit->spill[jit_regno(node->u.w)] = prev;
		break;
	    case jit_code_load:
		regno = jit_regno(node->u.w);
		if (register_change_p(node->link->next, node, regno) !=
		    jit_reg_change) {
		    /* spill not required due to optimizing common
		     * redundancy case of calling jit_get_reg/jit_unget_reg
		     * and then setting the register to the value it is
		     * already holding */
		    patch_register(node->link->next, node,
				   jit_regno_patch|regno, regno);
		    del_node(_jit->spill[regno], node->link);
		    del_node(prev, node);
		    node = prev;
		}
		_jit->spill[regno] = NULL;
		break;
	    case jit_code_movr:
		regno = jit_regno(node->u.w);
		if (simplify_movr(prev, node,
				  jit_kind_word, sizeof(jit_word_t)))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_movi:
		regno = jit_regno(node->u.w);
		if (simplify_movi(prev, node,
				  jit_kind_word, sizeof(jit_word_t)))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_movr_f:
		regno = jit_regno(node->u.w);
		if (simplify_movr(prev, node,
				  jit_kind_float32, sizeof(jit_float32_t)))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_movi_f:
		regno = jit_regno(node->u.w);
		if (simplify_movi(prev, node,
				  jit_kind_float32, sizeof(jit_float32_t)))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_movr_d:
		regno = jit_regno(node->u.w);
		if (simplify_movr(prev, node,
				  jit_kind_float64, sizeof(jit_float64_t)))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_movi_d:
		regno = jit_regno(node->u.w);
		if (simplify_movi(prev, node,
				  jit_kind_float64, sizeof(jit_float64_t)))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_ldxi_c:	case jit_code_ldxi_uc:
	    case jit_code_ldxi_s:	case jit_code_ldxi_us:
	    case jit_code_ldxi_i:	case jit_code_ldxi_ui:
	    case jit_code_ldxi_l:
	    case jit_code_ldxi_f:	case jit_code_ldxi_d:
		regno = jit_regno(node->u.w);
		if (simplify_ldxi(prev, node))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_stxi_c:	case jit_code_stxi_s:
	    case jit_code_stxi_i:	case jit_code_stxi_l:
	    case jit_code_stxi_f:	case jit_code_stxi_d:
		regno = jit_regno(node->u.w);
		if (simplify_stxi(prev, node))
		    simplify_spill(node = prev, regno);
		break;
	    case jit_code_callr:	case jit_code_calli:
		for (offset = 0; offset < _jit->reglen; offset++) {
		    if (!(jit_class(_rvs[offset].spec) & jit_class_sav)) {
			_jit->values[offset].kind = 0;
			++_jit->gen[offset];
		    }
		}
		break;
	    default:
		info = jit_classify(node->code);
		if (info & jit_cc_a0_jmp)
		    /* labels are not implicitly added when not taking
		     * a conditional branch */
		    goto reset;
		if (info & jit_cc_a0_chg) {
		    if (info & jit_cc_a0_rlh) {
			regno = jit_regno(node->u.q.l);
			_jit->values[regno].kind = 0;
			++_jit->gen[regno];
			regno = jit_regno(node->u.q.h);
			_jit->values[regno].kind = 0;
			++_jit->gen[regno];
		    }
		    else {
			regno = jit_regno(node->u.w);
			_jit->values[regno].kind = 0;
			++_jit->gen[regno];
		    }
		}
		if (info & jit_cc_a1_chg) {
		    regno = jit_regno(node->v.w);
		    _jit->values[regno].kind = 0;
		    ++_jit->gen[regno];
		}
		break;
	}
    }
}

static jit_int32_t
_register_change_p(jit_state_t *_jit, jit_node_t *node, jit_node_t *link,
		   jit_int32_t regno)
{
    jit_int32_t		value;

    for (; node != link; node = node->next) {
	switch (node->code) {
	    case jit_code_label:	case jit_code_prolog:
		/* lack of extra information so cannot say it is undefined */
		return (jit_reg_change);
	    case jit_code_callr:	case jit_code_calli:
		if (!(jit_class(_rvs[regno].spec) & jit_class_sav))
		    return (jit_reg_undef);
		break;
	    default:
		value = jit_classify(node->code);
		/* lack of extra information */
		if (value & jit_cc_a0_jmp)
		    return (jit_reg_change);
		else if ((value & (jit_cc_a0_reg|jit_cc_a0_chg)) ==
			 (jit_cc_a0_reg|jit_cc_a0_chg) &&
			 (((value & jit_cc_a0_rlh) &&
			   (node->u.q.l == regno || node->u.q.h == regno)) ||
			  (!(value & jit_cc_a0_rlh) &&
			   node->u.w == regno)))
		    return (jit_reg_change);
		else if ((value & jit_cc_a1_reg) && node->v.w == regno &&
			 (value & jit_cc_a1_chg))
		    return (jit_reg_change);
	}
    }

    return (jit_reg_static);
}

/* most of this could be done at the same time as generating jit, but
 * avoid complications on different cpu backends and patch spill/loads
 * here, by simulating jit generation */
static jit_bool_t
_spill_reglive_p(jit_state_t *_jit, jit_node_t *node, jit_int32_t regno)
{
    if (!jit_regset_tstbit(_jit->reglive, regno)) {
	mpz_set_ui(_jit->blockmask, 0);
	jit_regset_setbit(_jit->regmask, regno);
	jit_update(node->next, &_jit->reglive, &_jit->regmask);
	if (!jit_regset_tstbit(_jit->reglive, regno) &&
	    register_change_p(node->next, node->link, regno) != jit_reg_change)
	    return (0);
    }

    return (1);
}

static void
_patch_registers(jit_state_t *_jit)
{
    jit_node_t		*prev;
    jit_node_t		*node;
    jit_node_t		*next;
    jit_int32_t		 info;
    jit_int32_t		 spec;
    jit_int32_t		 regno;
    jit_int32_t		 value;

    _jit->function = NULL;

    jit_reglive_setup();
    for (prev = NULL, node = _jit->head; node; node = next) {
	next = node->next;

	info = jit_classify(node->code);
	jit_regarg_set(node, info);

	switch (node->code) {
	    case jit_code_save:
		regno = jit_regno(node->u.w);
		if (!spill_reglive_p(node, regno)) {
		    /* register is not live, just remove spill/reload */
		    jit_regarg_clr(node, info);
		    node->link->v.w = jit_regload_delete;
		    del_node(prev, node);
		    continue;
		}
		else {
		    /* try to find a free register of the same class */
		    spec = jit_class(_rvs[regno].spec) & ~jit_class_arg;
		    for (value = 0; value < _jit->reglen; value++) {
			if (value != regno &&
			    ((jit_class(_rvs[value].spec) & spec) &
			     ~jit_class_arg) == spec &&
			    !jit_regset_tstbit(_jit->regarg, value) &&
			    !spill_reglive_p(node, value))
			    break;
		    }
		    if (value < _jit->reglen) {
			jit_regarg_clr(node, info);
			patch_register(node->next, node->link,
				       jit_regno_patch|node->u.w,
				       jit_regno_patch|value);
			/* mark as live just in case there are nested
			 * register patches, so that next patch will
			 * not want to use the same register */
			jit_regset_setbit(_jit->reglive, value);
			/* register is not live, just remove spill/reload */
			node->link->v.w = jit_regload_isdead;
			del_node(prev, node);
			continue;
		    }
		    else {
			/* failed to find a free register */
			if (spec & jit_class_gpr) {
			    if (!_jit->function->regoff[regno])
				_jit->function->regoff[regno] =
				    jit_allocai(sizeof(jit_word_t));
#if __WORDSIZE == 32
			    node->code = jit_code_stxi_i;
#else
			    node->code = jit_code_stxi_l;
#endif
			}
			else {
			    node->code = jit_code_stxi_d;
			    if (!_jit->function->regoff[regno])
				_jit->function->regoff[regno] =
				    jit_allocai(sizeof(jit_float64_t));
			}
			node->u.w = _jit->function->regoff[regno];
			node->v.w = JIT_FP;
			node->w.w = regno;
			node->link = NULL;
		    }
		}
		break;
	    case jit_code_load:
		regno = jit_regno(node->u.w);
		if (node->v.w) {
		    if (node->v.w == jit_regload_isdead)
			jit_regset_clrbit(_jit->reglive, regno);
		    del_node(prev, node);
		    continue;
		}
		spec = jit_class(_rvs[regno].spec);
		if (spec & jit_class_gpr) {
#if __WORDSIZE == 32
		    node->code = jit_code_ldxi_i;
#else
		    node->code = jit_code_ldxi_l;
#endif
		}
		else
		    node->code = jit_code_ldxi_d;
		node->v.w = regno;
		node->v.w = JIT_FP;
		node->w.w = _jit->function->regoff[regno];
		node->link = NULL;
		break;
	    case jit_code_prolog:
		_jit->function = _jit->functions.ptr + node->w.w;
		break;
	    case jit_code_epilog:
		_jit->function = NULL;
		break;
	    default:
		break;
	}

	jit_regarg_clr(node, info);
	/* update register live state */
	jit_reglive(node);
	prev = node;
    }
}

static void
_patch_register(jit_state_t *_jit, jit_node_t *node, jit_node_t *link,
		jit_int32_t regno, jit_int32_t patch)
{
    jit_int32_t		value;

    for (; node != link; node = node->next) {
	value = jit_classify(node->code);
	if (value & jit_cc_a0_reg) {
	    if (value & jit_cc_a0_rlh) {
		if (node->u.q.l == regno)
		    node->u.q.l = patch;
		if (node->u.q.h == regno)
		    node->u.q.h = patch;
	    }
	    else {
		if (node->u.w == regno)
		    node->u.w = patch;
	    }
	}
	if ((value & jit_cc_a1_reg) && node->v.w == regno)
	    node->v.w = patch;
	if ((value & jit_cc_a2_reg) && node->w.w == regno)
	    node->w.w = patch;
    }
}

#if defined(__i386__) || defined(__x86_64__)
#  include "jit_x86.c"
#elif defined(__mips__)
#  include "jit_mips.c"
#elif defined(__arm__)
#  include "jit_arm.c"
#elif defined(__ppc__)
#  include "jit_ppc.c"
#elif defined(__sparc__)
#  include "jit_sparc.c"
#endif
