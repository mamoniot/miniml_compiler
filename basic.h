//By Monica Moniot
#ifndef BASIC__H_INCLUDE
#define BASIC__H_INCLUDE

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef uint8_t  byte;
typedef uint32_t uint;
typedef  int8_t   int8;
typedef uint8_t  uint8;
typedef  int16_t  int16;
typedef uint16_t uint16;
typedef  int32_t  int32;
typedef uint32_t uint32;
typedef  int64_t  int64;
typedef uint64_t uint64;

#define KILOBYTE (((int64)1)<<10)
#define MEGABYTE (((int64)1)<<20)
#define GIGABYTE (((int64)1)<<30)
#define TERABYTE (((int64)1)<<40)

#ifndef ASSERT
 #ifndef BASIC_NO_ASSERT
  #include <assert.h>
  #define ASSERT(is) assert(is)
  #define ASSERTL(is, err) assert(is && err)
 #else
  #define ASSERT(is) 0
  #define ASSERTL(is, err) 0
 #endif
#endif
#define NULLOP() 0

#define cast(type, value) ((type)(value))
#define talloc(type, size) ((type*)malloc(sizeof(type)*(size)))
#define from_cstr(str) str, strlen(str)
#define ptr_add(type, ptr, n) ((type*)((byte*)(ptr) + (n)))
#define ptr_dist(ptr0, ptr1) ((int64)((byte*)(ptr1) - (byte*)(ptr0)))
#define memzero(ptr, size) memset(ptr, 0, size)

#define MACRO_CAT_(a, b) a ## b
#define MACRO_CAT(a, b) MACRO_CAT_(a, b)
#define UNIQUE_NAME(prefix) MACRO_CAT(prefix, __LINE__)

#define for_each_lt(name, size) int32 UNIQUE_NAME(name) = (size); for(int32 name = 0; name < UNIQUE_NAME(name); name += 1)
#define for_each_lt_bw(name, size) for(int32 name = (size) - 1; name >= 0; name -= 1)
#define for_each_in_range(name, r0, r1) int32 UNIQUE_NAME(name) = (r1); for(int32 name = (r0); name <= UNIQUE_NAME(name); name += 1)
#define for_each_in_range_bw(name, r0, r1) int32 UNIQUE_NAME(name) = (r0); for(int32 name = (r1); name >= UNIQUE_NAME(name); name -= 1)
#define for_ever(name) for(int32 name = 0;; name += 1)

#ifdef __cplusplus
 #define swap(v0, v1) auto UNIQUE_NAME(__t) = *(v0); *(v0) = *(v1); *(v1) = UNIQUE_NAME(__t)
 #define for_each_in(name, array, size) auto UNIQUE_NAME(name) = (array) + (size); for(auto name = (array); name != UNIQUE_NAME(name); name += 1)
 #define for_each_in_bw(name, array, size) auto UNIQUE_NAME(name) = (array) - 1; for(auto name = (array) + (size) - 1; name != UNIQUE_NAME(name); name -= 1)
#endif

#define tape_destroy(tape_ptr) (*(tape_ptr) ? free(__tape_base(tape_ptr)) : 0)
#define tape_push(tape_ptr, v) (__tape_may_grow((tape_ptr), 1), (*(tape_ptr))[__tape_base(tape_ptr)[1]++] = (v))
#define tape_append tape_push
#define tape_pop(tape_ptr) ((*(tape_ptr))[--__tape_base(tape_ptr)[1]])
#define tape_size(tape_ptr) ((*(tape_ptr)) ? __tape_base(tape_ptr)[1] : 0)
#define tape_reserve(tape_ptr, n) (__tape_may_grow((tape_ptr), (n)), __tape_base(tape_ptr)[1] += (n), &((*(tape_ptr))[__tape_base(tape_ptr)[1] - (n)]))
#define tape_release(tape_ptr, n) (__tape_base(tape_ptr)[1] -= n)
#define tape_get_last(tape_ptr) ((*(tape_ptr))[__tape_base(tape_ptr)[1] - 1])
#define tape_reset(tape_ptr) (*(tape_ptr) ? (__tape_base(tape_ptr)[1] = 0) : 0)

#define __tape_base(tape_ptr) ((uint32*)(*(tape_ptr)) - 2)
#define __tape_may_grow(tape_ptr, n) ((*(tape_ptr) == 0 || (__tape_base(tape_ptr)[1] + n >= __tape_base(tape_ptr)[0])) ? ((*(void**)(tape_ptr)) = __tape_grow((void*)*(tape_ptr), n, sizeof(**(tape_ptr)))) : 0)

static void* __tape_grow(void* tape, uint32 inc, uint32 item_size) {
	uint32* ptr;
	uint32 new_capacity;
	if(tape) {
		uint32* tape_base = __tape_base(&tape);
		uint32 dbl_cur = 2*tape_base[0];
		uint32 min_needed = tape_base[1] + inc;
		new_capacity = dbl_cur > min_needed ? dbl_cur : min_needed;
		ptr = (uint32*)realloc(tape_base, item_size*new_capacity + 2*sizeof(uint32));
	} else {
		new_capacity = inc;
		ptr = (uint32*)malloc(item_size*new_capacity + 2*sizeof(uint32));
		ptr[1] = 0;
	}
	if(ptr) {
		ptr[0] = new_capacity;
		return (void*)(ptr + 2);
	} else {
		return (void*)(2*sizeof(uint32)); // try to force a NULL pointer exception later
	}
}

#endif
