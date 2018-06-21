/* Copyright 2010-2011,2018
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>

#include <libguile.h>

void test_ffi_v_ (void);
void test_ffi_v_ (void)
{
  return;
}

void test_ffi_v_u8 (uint8_t a);
void test_ffi_v_u8 (uint8_t a)
{
  return;
}

void test_ffi_v_s64 (int64_t a);
void test_ffi_v_s64 (int64_t a)
{
  return;
}

int8_t test_ffi_s8_ (void);
int8_t test_ffi_s8_ (void)
{
  return -100;
}
int8_t test_ffi_s8_u8 (uint8_t a);
int8_t test_ffi_s8_u8 (uint8_t a)
{
  return -100 + a;
}

int8_t test_ffi_s8_s64 (int64_t a);
int8_t test_ffi_s8_s64 (int64_t a)
{
  return -100 + a;
}

uint8_t test_ffi_u8_ (void);
uint8_t test_ffi_u8_ (void)
{
  return 200;
}

uint8_t test_ffi_u8_u8 (uint8_t a);
uint8_t test_ffi_u8_u8 (uint8_t a)
{
  return 200 + a;
}

uint8_t test_ffi_u8_s64 (int64_t a);
uint8_t test_ffi_u8_s64 (int64_t a)
{
  return 200 + a;
}

int16_t test_ffi_s16_ (void);
int16_t test_ffi_s16_ (void)
{
  return -20000;
}

int16_t test_ffi_s16_u8 (uint8_t a);
int16_t test_ffi_s16_u8 (uint8_t a)
{
  return -20000 + a;
}

int16_t test_ffi_s16_s64 (int64_t a);
int16_t test_ffi_s16_s64 (int64_t a)
{
  return -20000 + a;
}

uint16_t test_ffi_u16_ (void);
uint16_t test_ffi_u16_ (void)
{
  return 40000;
}

uint16_t test_ffi_u16_u8 (uint8_t a);
uint16_t test_ffi_u16_u8 (uint8_t a)
{
  return 40000 + a;
}

uint16_t test_ffi_u16_s64 (int64_t a);
uint16_t test_ffi_u16_s64 (int64_t a)
{
  return 40000 + a;
}

int32_t test_ffi_s32_ (void);
int32_t test_ffi_s32_ (void)
{
  return -2000000000;
}

int32_t test_ffi_s32_u8 (uint8_t a);
int32_t test_ffi_s32_u8 (uint8_t a)
{
  return -2000000000 + a;
}

int32_t test_ffi_s32_s64 (int64_t a);
int32_t test_ffi_s32_s64 (int64_t a)
{
  return -2000000000 + a;
}

uint32_t test_ffi_u32_ (void);
uint32_t test_ffi_u32_ (void)
{
  return 4000000000U;
}

uint32_t test_ffi_u32_u8 (uint8_t a);
uint32_t test_ffi_u32_u8 (uint8_t a)
{
  return 4000000000U + a;
}

uint32_t test_ffi_u32_s64 (int64_t a);
uint32_t test_ffi_u32_s64 (int64_t a)
{
  return 4000000000U + a;
}

/* FIXME: use 64-bit literals */
int64_t test_ffi_s64_ (void);
int64_t test_ffi_s64_ (void)
{
  return -2000000000;
}

int64_t test_ffi_s64_u8 (uint8_t a);
int64_t test_ffi_s64_u8 (uint8_t a)
{
  return -2000000000 + a;
}

int64_t test_ffi_s64_s64 (int64_t a);
int64_t test_ffi_s64_s64 (int64_t a)
{
  return -2000000000 + a;
}

uint64_t test_ffi_u64_ (void);
uint64_t test_ffi_u64_ (void)
{
  return 4000000000UL;
}

uint64_t test_ffi_u64_u8 (uint8_t a);
uint64_t test_ffi_u64_u8 (uint8_t a)
{
  return 4000000000UL + a;
}

uint64_t test_ffi_u64_s64 (int64_t a);
uint64_t test_ffi_u64_s64 (int64_t a)
{
  return 4000000000UL + a;
}


int64_t test_ffi_sum (int8_t a, int16_t b,
                          int32_t c, int64_t d);
int64_t test_ffi_sum (int8_t a, int16_t b,
                          int32_t c, int64_t d)
{
  return d + c + b + a;
}


int64_t test_ffi_sum_many (uint8_t a, uint16_t b,
                           uint32_t c, uint64_t d,
                           int8_t e, int16_t f,
                           int32_t g, int64_t h,
                           int8_t i, int16_t j,
                           int32_t k, int64_t l);
int64_t test_ffi_sum_many (uint8_t a, uint16_t b,
                           uint32_t c, uint64_t d,
                           int8_t e, int16_t f,
                           int32_t g, int64_t h,
                           int8_t i, int16_t j,
                           int32_t k, int64_t l)
{
  return l + k + j + i + h + g + f + e + d + c + b + a;
}


struct foo
{
  int8_t a;
  int16_t b;
  int32_t c;
  int64_t d;
};
int64_t test_ffi_sum_struct (struct foo foo);
int64_t test_ffi_sum_struct (struct foo foo)
{
  return foo.d + foo.c + foo.b + foo.a;
}


void* test_ffi_memcpy (void *dest, void *src, int32_t n);
void* test_ffi_memcpy (void *dest, void *src, int32_t n)
{
  return memcpy (dest, src, n);
}

int test_ffi_callback_1 (int (*f) (int), int x);
int test_ffi_callback_1 (int (*f) (int), int x)
{
  return f (x) + 7;
}

double test_ffi_callback_2 (double (*f) (float, int, double),
			    float x, int y, double z);
double test_ffi_callback_2 (double (*f) (float, int, double),
			    float x, int y, double z)
{
  return f (x, y, z);
}
