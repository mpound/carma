#ifndef hash_h
#define hash_h

#include "carma/szaarrayutils/freelist.h"
#include "carma/szaarrayutils/stringmem.h"

/*
 * The following macro can be used to prototype or define a
 * function that deletes the data of a symbol-table entry.
 *
 * Input:
 *  app_data void *  The new_HashTable() app_data argument.
 *  code      int    The Symbol::code argument.
 *  sym_data void *  The Symbol::data argument to be deleted.
 * Output:
 *  return  void * The deleted data (always return NULL).
 */
#define SYM_DEL_FN(fn) void *(fn)(void *app_data, int code, void *sym_data)

/*
 * The following macro can be used to prototype or define a
 * function that deletes the application-data of a hash-table.
 *
 * Input:
 *  data    void * The new_HashTable() 'app_data' argument to be
 *                 deleted.
 * Output:
 *  return  void * The deleted data (always return NULL).
 */
#define HASH_DEL_FN(fn) void *(fn)(void *app_data)

/*
 * The following is a container for recording the context
 * of a symbol in a manner that is independant of the particular
 * symbol-table implementation. Each hash-table entry contains
 * the following user supplied parameters:
 *
 * 1. An optional integral parameter 'code'. This is useful for
 *    enumerating a symbol or for describing what type of data
 *    or function is stored in the symbol.
 *
 * 2. An optional generic function pointer. This is useful for
 *    associating functions with names. The user is responsible
 *    for casting between the generic function type and the
 *    actual function type. The code field could be used to
 *    enumerate what type of function to cast to.
 *
 * 3. An optional generic pointer to a static or heap-allocated
 *    object. It is up to the user to cast this back to the
 *    appropriate object type. Again, the code field could be used
 *    to describe what type of object is stored there.
 *    If the object is dynamically allocated and should be discarded
 *    when the symbol is deleted from the symbol table, send a
 *    destructor function to have it deleted automatically.
 */
typedef struct {
  char *name;           /* The name of the symbol */
  int code;             /* Application supplied integral code */
  void (*fn)(void);     /* Application supplied generic function */
  void *data;           /* Application supplied context data */
  SYM_DEL_FN(*del_fn);  /* Data destructor function */
} Symbol;

/*
 * HashNode's and HashTable's are small objects. Separately allocating
 * many such objects would normally cause memory fragmentation. To
 * counter this, HashMemory objects are used. These contain
 * dedicated free-lists formed from large dynamically allocated arrays
 * of objects. One HashMemory object can be shared between multiple hash
 * tables (within a single thread).
 */
/*
 * The following container object contains free-lists to be used
 * for allocation of HashTable containers and nodes.
 */
struct HashMemory {
  FreeList *hash_memory;    /* HashTable free-list */
  FreeList *node_memory;    /* HashNode free-list */
  StringMem *string_memory; /* Memory used to allocate hash strings */
};

  /* Create a free-list for allocation of hash tables and their nodes */

HashMemory *new_HashMemory(int hash_count, int node_count);

  /* Delete a redundant free-list if not being used */

HashMemory *del_HashMemory(HashMemory *mem, int force);

/*
 * Display the number of active hash-tables and hash nodes that are 
 * currently allocated from a given freelist
 */
void show_HashMemory(HashMemory *mem);

/*
 * Define a hash symbol-table entry.
 * See symbol.h for the definition of the Symbol container type.
 */
typedef struct HashNode HashNode;
struct HashNode {
  Symbol symbol;       /* The symbol stored in the hash-entry */
  HashNode *next;      /* The next hash-table entry in a bucket list */
};

/*
 * Each hash-table bucket contains a linked list of entries that
 * hash to the same bucket.
 */
typedef struct {
  HashNode *head;   /* The head of the bucket hash-node list */
  int count;        /* The number of entries in the list */
} HashBucket;

/*
 * A hash-table consists of 'size' hash buckets.
 * Note that the HashTable typedef for this struct is contained in hash.h.
 */
struct HashTable {
  unsigned ref;         /* The reference count of the table */
  HashMemory *mem;      /* HashTable free-list */
  int internal_mem;     /* True if 'mem' was allocated by new_HashTable() */
  int case_sensitive;   /* True if case is significant in lookup keys */
  int size;             /* The number of hash buckets */
  HashBucket *bucket;   /* An array of 'size' hash buckets */
  int (*keycmp)(const char *, const char *); /* Key comparison function */
  void *app_data;       /* Application-provided data */
  HASH_DEL_FN(*del_fn); /* Application-provided 'app_data' destructor */
};

/*
 * Enumerate case-sensitivity options.
 */
typedef enum {
  IGNORE_CASE,     /* Ignore case when looking up symbols */
  HONOUR_CASE      /* Honor case when looking up symbols */
} HashCase;

  /* Create a new hash-table */

HashTable *new_HashTable(HashMemory *mem, int size, HashCase hcase,
			 void *app_data, HASH_DEL_FN(*del_fn));

  /* Increment the reference count of a list to prevent untimely deletion */

HashTable *ref_HashTable(HashTable *hash);

  /* Delete a reference to a hash-table */

HashTable *del_HashTable(HashTable *hash);

  /* Add an entry to a hash table */

Symbol *new_HashSymbol(HashTable *hash, char *key, int code, void (*fn)(void),
		       void *data, SYM_DEL_FN(*del_fn));

  /* Remove and delete all the entries in a given hash table */

int clear_HashTable(HashTable *hash);

  /* Remove and delete a given hash-table entry */

Symbol *del_HashSymbol(HashTable *hash, char *key);

  /* Lookup a given hash-table entry */

Symbol *find_HashSymbol(HashTable *hash, char *key);

  /* Display the contents of a hash table to standard output */

void show_HashTable(HashTable *hash, int summarize);

  /* Execute a given function on each entry of a hash table, returning */
  /*  before completion if the specified function returns non-zero. */

#define HASH_SCAN_FN(fn)  int (fn)(Symbol *sym, void *context)

int scan_HashTable(HashTable *hash, HASH_SCAN_FN(*scan_fn), void *context);

#endif
