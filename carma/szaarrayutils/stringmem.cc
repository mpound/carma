#include <stdlib.h>
#include <stdio.h>

#include "carma/szaarrayutils/stringmem.h"
#include "carma/szaarrayutils/freelist.h"
#include "carma/szaarrayutils/lprintf.h"

struct StringMem {
  unsigned long nmalloc;  /* The number of strings allocated with malloc */
  FreeList *fl;           /* The free-list */
};

/*.......................................................................
 * Create a string free-list container and the first block of its free-list.
 *
 * Input:
 *  caller     const char *  The name of the calling function.
 *  blocking_factor   int    The blocking_factor argument specifies how
 *                           many strings of length SM_STRLEN
 *                           bytes (see stringmem.h) are allocated in each
 *                           free-list block.
 *                           For example if blocking_factor=64 and
 *                           SM_STRLEN=16, then each new
 *                           free-list block will take 1K of memory.
 * Output:
 *  return      StringMem *  The new free-list container, or NULL on
 *                           error.
 */
StringMem *new_StringMem(const char *caller, unsigned blocking_factor)
{
  StringMem *sm;    /* The container to be returned. */
/*
 * Check arguments.
 */
  if(blocking_factor < 1) {
    lprintf(stderr, "new_StringMem (%s): Bad blocking factor (%d).\n",
	    caller ? caller : "unknown caller", blocking_factor);
    return NULL;
  };
/*
 * Allocate the container.
 */
  sm = (StringMem *) malloc(sizeof(StringMem));
  if(!sm) {
    lprintf(stderr, "new_StringMem (%s): Insufficient memory.\n",
	    caller ? caller : "unknown caller");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize
 * the container at least up to the point at which it can safely
 * be passed to del_StringMem().
 */
  sm->nmalloc = 0;
  sm->fl = NULL;
/*
 * Allocate the free-list.
 */
  sm->fl = new_FreeList(caller, SM_STRLEN, blocking_factor);
  if(!sm->fl)
    return del_StringMem(caller, sm, 1);
/*
 * Return the free-list container.
 */
  return sm;
}

/*.......................................................................
 * Delete a string free-list.
 *
 * Input:
 *  caller  const char *  The name of the calling function.
 *  sm       StringMem *  The string free-list to be deleted, or NULL.
 *  force          int    If force==0 then del_StringMem() will complain
 *                         and refuse to delete the free-list if any
 *                         of nodes have not been returned to the free-list.
 *                        If force!=0 then del_StringMem() will not check
 *                         whether any nodes are still in use and will
 *                         always delete the list.
 * Output:
 *  return   StringMem *  Always NULL (even if the list couldn't be
 *                        deleted).
 */
StringMem *del_StringMem(const char *caller, StringMem *sm, int force)
{
  if(sm) {
/*
 * Check whether any strings have not been returned to the free-list.
 */
    if(!force && (sm->nmalloc > 0 || busy_FreeListNodes(sm->fl) > 0)) {
      lprintf(stderr, "del_StringMem (%s): Free-list in use.\n",
	      caller ? caller : "unknown caller");
      return NULL;
    };
/*
 * Delete the free-list.
 */
    sm->fl = del_FreeList(caller, sm->fl, force);
/*
 * Delete the container.
 */
    free(sm);
  };
  return NULL;
}

/*.......................................................................
 * Allocate an array of 'length' chars.
 *
 * Input:
 *  caller const char *  The name of the calling function.
 *  sm      StringMem *  The string free-list to allocate from.
 *  length     size_t    The length of the new string (including '\0').
 * Output:
 *  return       char *  The new string or NULL on error.
 */
char *new_StringMemString(const char *caller, StringMem *sm, size_t length)
{
  char *string;   /* The string to be returned */
  int was_malloc; /* True if malloc was used to allocate the string */
/*
 * Check arguments.
 */
  if(!sm) {
    lprintf(stderr, "new_StringMemString (%s): NULL StringMem.\n",
	    caller ? caller : "unknown caller");
    return NULL;
  };
  if(length < 1) {
    lprintf(stderr, "new_StringMemString (%s): Bad length (%lu).\n",
	    caller ? caller : "unknown caller", (unsigned long) length);
    return NULL;
  };
/*
 * Allocate the new node from the free list if possible.
 */
  if(length < SM_STRLEN) {
    string = (char* )new_FreeListNode(caller, sm->fl);
    if(!string)
      return NULL;
    was_malloc = 0;
  } else {
    string = (char* )malloc(length+1); /* Leave room for the flag byte */
    if(!string) {
      lprintf(stderr, "new_StringMemString (%s): Insufficient memory.\n",
	      caller ? caller :"unknown caller");
      return NULL;
    };
/*
 * Count malloc allocations.
 */
    was_malloc = 1;
    sm->nmalloc++;
  };
/*
 * Use the first byte of the string to record whether the string was
 * allocated with malloc or from the free-list. Then return the rest
 * of the string for use by the user.
 */
  string[0] = (char) was_malloc;
  return string + 1;
}

/*.......................................................................
 * Free a string that was previously returned by new_StringMemString().
 *
 * Input:
 *  caller const char *  The name of the calling function.
 *  sm      StringMem *  The free-list from which the string was originally
 *                       allocated.
 *  s            char *  The string to be returned to the free-list, or NULL.
 * Output:
 *  return       char *  Always NULL.
 */
char *del_StringMemString(const char *caller, StringMem *sm, char *s)
{
  int was_malloc;  /* True if the string originally came from malloc() */
/*
 * Is there anything to be deleted?
 */
  if(s) {
    if(!sm) {
      lprintf(stderr, "del_StringMem (%s): NULL StringMem argument.\n",
	      caller ? caller : "unknown caller");
      return NULL;
    };
/*
 * Retrieve the true string pointer. This is one less than the one
 * returned by new_StringMemString() because the first byte of the
 * allocated memory is reserved by new_StringMemString as a flag byte
 * to say whether the memory was allocated from the free-list or directly
 * from malloc().
 */
    s--;
/*
 * Get the origination flag.
 */
    was_malloc = s[0];
    if(was_malloc) {
      free(s);
      s = NULL;
      sm->nmalloc--;
    } else {
      s = (char* )del_FreeListNode(caller, sm->fl, s);
    };
  };
  return NULL;
}


