#include <stdlib.h>
#include <string.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/script.h"
#include "carma/szaarrayutils/freelist.h"


/*
 * The immutable values of string datatypes are allocated contiguously
 * from a pool of large char arrays of length STR_SEG_SIZE.
 */
#define STR_SEG_SIZE 256

/*
 * Encapsulate a string segment buffer.
 */
typedef struct {
  char buf[STR_SEG_SIZE];  /* A string segment buffer from which to */
                           /*  contiguously allocate '\0' terminated strings */
} StringPoolBuf;

struct StringPool {     /* Typedef'd to StringPool in stringpool.h */
  FreeList *buf_mem;    /* A free-list of StringPoolBuf objects */
  StringPoolBuf *seg;   /* The segment that is currently being used */
                        /*  for allocating strings */
  unsigned bufpos;      /* The next string can be allocated starting from */
                        /*  seg->buf[bufpos]. If the required length */
                        /*  of the new string is >= STR_SEG_SIZE-bufpos, */
                        /*  allocate a new buffer. */
};

/*.......................................................................
 * Create an expandable memory pool from which to allocate strings.
 * Memory is allocated sequentially from large character arrays. The
 * memory used by a string can not be reclaimed until the whole pool
 * is cleared.
 *
 * Output:
 *  return  StringPool *   The new string allocator, or NULL on error.
 */
StringPool *new_StringPool(void)
{
  StringPool *sp;   /* The object to be returned */
/*
 * Allocate the container.
 */
  sp = (StringPool *) malloc(sizeof(StringPool));
  if(!sp) {
    lprintf(stderr, "new_StringPool: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be
 * passed to del_StringPool().
 */
  sp->buf_mem = NULL;
  sp->seg = NULL;
  sp->bufpos = 0;
/*
 * Allocate a freelist of string segment buffers.
 */
  sp->buf_mem = new_FreeList("new_StringPool", sizeof(StringPoolBuf), 1);
  if(!sp->buf_mem)
    return del_StringPool(sp);
  return sp;
}

/*.......................................................................
 * Delete a pool of strings.
 *
 * Input:
 *  sp     StringPool *  The object to be deleted.
 * Output:
 *  return StringPool *  The deleted object (always NULL).
 */ 
StringPool *del_StringPool(StringPool *sp)
{
  if(sp) {
    sp->buf_mem = del_FreeList("del_StringPool", sp->buf_mem, 1);
    free(sp);
  };
  return NULL;
}

/*.......................................................................
 * Clear a string pool of allocated strings.
 *
 * Input:
 *  sp     StringPool *  The string pool to be cleared.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
int clr_StringPool(StringPool *sp)
{
  if(!sp) {
    lprintf(stderr, "clr_StringPool: NULL pool.\n");
    return 1;
  };
/*
 * Return all string segments to the free-list.
 */
  rst_FreeList(sp->buf_mem);
  sp->seg = NULL;
  sp->bufpos = 0;
  return 0;
}

/*.......................................................................
 * Allocate a string from a string pool.
 *
 * Input:
 *  sp    StringPool *  The string pool to allocate from.
 *  string      char *  The string to be copied.
 * Output:
 *  return      char *  A new copy of 'string'.
 */
char *new_StringPool_string(StringPool *sp, char *string)
{
  size_t length; /* The length of string[] including the '\0' terminator */
  char *copy;    /* The new copy of string[] */
/*
 * Check inputs.
 */
  if(!sp || !string) {
    lprintf(stderr, "new_StringPool_string: NULL argument(s).\n");
    return NULL;
  };
/*
 * String too large?
 */
  length = strlen(string) + 1;
  if(length > STR_SEG_SIZE) {
    lprintf(stderr, "new_StringPool_string: String too long.\n");
    return NULL;
  };
/*
 * Do we need a new string segment?
 */
  if(!sp->seg || (STR_SEG_SIZE - sp->bufpos) < strlen(string)+1) {
    StringPoolBuf *seg = (StringPoolBuf* )new_FreeListNode("new_StringPool_string", sp->buf_mem);
    if(!seg)
      return NULL;
    sp->seg = seg;
    sp->bufpos = 0;
  };
/*
 * Allocate the string from the current string segment buffer.
 */
  copy = sp->seg->buf + sp->bufpos;
  strcpy(copy, string);
  sp->bufpos += length;
  return copy;
}
