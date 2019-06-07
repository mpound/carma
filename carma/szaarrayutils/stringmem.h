#ifndef stringmem_h
#define stringmem_h

typedef struct StringMem StringMem;

/*
 * Applications that dynamically allocate lots of small strings
 * run the risk of significantly fragmenting the heap. This module
 * aims to reduce this risk by allocating large arrays of small fixed
 * length strings, arranging them as a free-list and allowing
 * callers to allocate from the list. Strings that are too long
 * to be allocated from the free-list are allocated from the heap.
 * Since typical implementations of malloc() eat up a minimum of
 * 16 bytes per call to malloc() [because of alignment and space
 * management constraints] it makes sense to set the free-list
 * string size to 16 bytes. Note that unlike malloc() which typically
 * keeps 8 bytes per allocation for its own use, our allocator will
 * return all but one of the 16 bytes for use. One hidden byte of overhead
 * is reserved for flagging whether the string was allocated directly
 * from malloc or from the free-list.
 */

/*
 * Set the length of each free-list string. The longest string that
 * will be returned without calling malloc() will be one less than
 * this number.
 */
#define SM_STRLEN 16

/*
 * Create a string free-list container and the first block of its free-list.
 */
StringMem *new_StringMem(const char *caller, unsigned blocking_factor);

/*
 * Delete a string free-list.
 */
StringMem *del_StringMem(const char *caller, StringMem *sm, int force);

/*
 * Allocate an array of 'length' chars.
 */
char *new_StringMemString(const char *caller, StringMem *sm, size_t size);

/*
 * Free a string that was previously returned by new_StringMemString().
 */
char *del_StringMemString(const char *caller, StringMem *sm, char *s);

#endif
