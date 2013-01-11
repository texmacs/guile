/*
 * Copyright (C) 2013  Free Software Foundation, Inc.
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

/*
 * Prototypes
 */
#define new_note(u, v, w, x)	_new_note(_jit, u, v, w, x)
static void _new_note(jit_state_t*,jit_int32_t,char*,jit_int32_t,jit_int32_t);
#define note_insert_index(u)	_note_insert_index(_jit, u)
static jit_int32_t _note_insert_index(jit_state_t*,jit_int32_t);
#define note_search_index(u)	_note_search_index(_jit, u)
static jit_int32_t _note_search_index(jit_state_t*,jit_int32_t);
static jit_int32_t offset_insert_index(jit_note_t*,jit_int32_t);
static jit_int32_t offset_search_index(jit_note_t*,jit_int32_t);

/*
 * Implementation
 */
void
jit_init_note(void)
{
}

void
jit_finish_note(void)
{
}

void
_jit_set_note(jit_state_t *_jit,
	      char *name, int lineno, jit_int32_t offset)
{
    jit_note_t		*note;
    jit_int32_t		 index;

    index = note_insert_index(offset);
    if (index >= _jit->note.length || _jit->note.ptr[index].name != name)
	new_note(index, name, lineno, offset);
    else {
	note = _jit->note.ptr + index;
	index = offset_insert_index(note, offset);
	if (note->offsets[index] == offset) {
	    /* common case if no code was generated for several source lines */
	    if (note->linenos[index] < lineno)
		note->linenos[index] = lineno;
	}
	else if (note->linenos[index] == lineno) {
	    /* common case of extending entry */
	    if (note->offsets[index] > offset)
		note->offsets[index] = offset;
	}
	else {
	    /* line or offset changed */
	    if ((note->length & 15) == 0) {
		note->linenos = realloc(note->linenos, (note->length + 17) *
					sizeof(jit_int32_t));
		note->offsets = realloc(note->offsets, (note->length + 17) *
					sizeof(jit_int32_t));
	    }
	    if (index < note->length) {
		memmove(note->linenos + index + 1, note->linenos + index,
			sizeof(jit_int32_t) * (note->length - index));
		memmove(note->offsets + index + 1, note->offsets + index,
			sizeof(jit_int32_t) * (note->length - index));
	    }
	    note->linenos[index] = lineno;
	    note->offsets[index] = offset;
	    ++note->length;
	}
    }
}

jit_bool_t
_jit_get_note(jit_state_t *_jit, jit_uint8_t *code,
	      char **name, jit_int32_t *lineno)
{
    jit_note_t		*note;
    jit_int32_t		 index;
    jit_int32_t		 offset;

    if (code < _jit->code.ptr || code >= _jit->pc.uc)
	return (0);
    offset = code - _jit->code.ptr;
    if ((index = note_search_index(offset)) >= _jit->note.length)
	return (0);
    if (index == 0 && offset < _jit->note.ptr[0].offsets[0])
	return (0);
    note = _jit->note.ptr + index;
    if ((index = offset_search_index(note, offset)) >= note->length)
	return (0);

    if (name)
	*name = note->name;
    if (lineno)
	*lineno = note->linenos[index];

    return (1);
}

static void
_new_note(jit_state_t *_jit, jit_int32_t index,
	  char *name, jit_int32_t lineno, jit_int32_t offset)
{
    jit_note_t		*note;

    if (_jit->note.ptr == NULL)
	_jit->note.ptr = malloc(16 * sizeof(jit_note_t));
    else if ((_jit->note.length & 15) == 0)
	_jit->note.ptr = realloc(_jit->note.ptr,
				 (_jit->note.length + 17) * sizeof(jit_note_t));

    if (index < _jit->note.length)
	memmove(_jit->note.ptr + index + 1, _jit->note.ptr + index,
		sizeof(jit_note_t) * (_jit->note.length - index));
    note = _jit->note.ptr + index;
    ++_jit->note.length;

    note->name = name;
    note->length = 1;
    note->linenos = malloc(16 * sizeof(jit_int32_t));
    note->linenos[0] = lineno;
    note->offsets = malloc(16 * sizeof(jit_int32_t));
    note->offsets[0] = offset;
}

static jit_int32_t
_note_insert_index(jit_state_t *_jit, jit_int32_t offset)
{
    jit_int32_t		 bot;
    jit_int32_t		 top;
    jit_int32_t		 index;
    jit_note_t		*notes;

    bot = 0;
    top = _jit->note.length;
    if ((notes = _jit->note.ptr) == NULL)
	return (0);
    for (index = (bot + top) >> 1; bot < top; index = (bot + top) >> 1) {
	if (offset < *notes[index].offsets)
	    top = index;
	else
	    bot = index + 1;
    }

    return ((bot + top) >> 1);
}

static jit_int32_t
_note_search_index(jit_state_t *_jit, jit_int32_t offset)
{
    jit_int32_t		 bot;
    jit_int32_t		 top;
    jit_int32_t		 index;
    jit_note_t		*notes;

    bot = 0;
    top = _jit->note.length;
    if ((notes = _jit->note.ptr) == NULL)
	return (0);
    for (index = (bot + top) >> 1; bot < top; index = (bot + top) >> 1) {
	if (offset < *notes[index].offsets)
	    top = index;
	/* offset should be already verified to be in range */
	else if (index == _jit->note.length - 1 ||
		 (offset >= *notes[index].offsets &&
		  offset < *notes[index + 1].offsets))
	    break;
	else
	    bot = index + 1;
    }

    return (index);
}

static jit_int32_t
offset_insert_index(jit_note_t *note, jit_int32_t offset)
{
    jit_int32_t		 bot;
    jit_int32_t		 top;
    jit_int32_t		 index;
    jit_int32_t		*offsets;

    bot = 0;
    top = note->length;
    offsets = note->offsets;
    for (index = (bot + top) >> 1; bot < top; index = (bot + top) >> 1) {
	if (offset < offsets[index])
	    top = index;
	else
	    bot = index + 1;
    }

    return ((bot + top) >> 1);
}

static jit_int32_t
offset_search_index(jit_note_t *note, jit_int32_t offset)
{
    jit_int32_t		 bot;
    jit_int32_t		 top;
    jit_int32_t		 index;
    jit_int32_t		*offsets;

    bot = 0;
    top = note->length;
    offsets = note->offsets;
    for (index = (bot + top) >> 1; bot < top; index = (bot + top) >> 1) {
	if (offset < offsets[index])
	    top = index;
	/* offset should be already verified to be in range */
	else if (index == note->length - 1 ||
		 (offset >= offsets[index] && offset < offsets[index + 1]))
	    break;
	else
	    bot = index + 1;
    }

    return (index);
}
