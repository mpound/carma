#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "carma/szaarrayutils/hash.h"
#include "carma/szaarrayutils/lprintf.h"


static HashNode *del_HashNode(HashTable *hash, HashNode *node);
static HashNode *new_HashNode(HashTable *hash, char *name, int code,
		      void (*fn)(void), void *data, SYM_DEL_FN(*del_fn));
static HashNode *find_HashNode(HashTable *hash, HashBucket *bucket,
				char *name, HashNode **prev);
static HashBucket *find_HashBucket(HashTable *hash, char *name);
static int lower_strcmp(const char *node_key, const char *look_key);

/*.......................................................................
 * Allocate a free-list for use in allocating hash tables and their nodes.
 *
 * Input:
 *  list_count    int    The number of HashTable containers per free-list
 *                       block.
 *  node_count    int    The number of HashTable nodes per free-list block.
 * Output:
 *  return HashMemory *  The new free-list for use in allocating hash tables
 *                       and their nodes.
 */
HashMemory *new_HashMemory(int hash_count, int node_count)
{
  HashMemory *mem;
/*
 * Allocate the free-list container.
 */
  mem = (HashMemory *) malloc(sizeof(HashMemory));
  if(!mem) {
    lprintf(stderr, "new_HashMemory: Insufficient memory.\n");
    return NULL;
  };
/*
 * Initialize the container at least up to the point at which it can
 * safely be passed to del_HashMemory().
 */
  mem->hash_memory = NULL;
  mem->node_memory = NULL;
  mem->string_memory = NULL;
/*
 * Allocate the two free-lists.
 */
  mem->hash_memory = new_FreeList("new_HashMemory", sizeof(HashTable),
				  hash_count);
  if(!mem->hash_memory)
    return del_HashMemory(mem, 1);
  mem->node_memory = new_FreeList("new_HashMemory", sizeof(HashNode),
				  node_count);
  if(!mem->node_memory)
    return del_HashMemory(mem, 1);
  mem->string_memory = new_StringMem("new_HashMemory", 64);
  if(!mem->string_memory)
    return del_HashMemory(mem, 1);
/*
 * Return the free-list container.
 */
  return mem;
}

/*.......................................................................
 * Delete a HashTable free-list. An error will be displayed if the list is
 * still in use and the deletion will be aborted.
 *
 * Input:
 *  mem    HashMemory *  The free-list container to be deleted.
 *  force         int    If force==0 then del_HashMemory() will complain
 *                        and refuse to delete the free-list if any
 *                        of nodes have not been returned to the free-list.
 *                       If force!=0 then del_HashMemory() will not check
 *                        whether any nodes are still in use and will
 *                        always delete the list.
 * Output:
 *  return HashMemory *  Always NULL (even if the memory could not be
 *                       deleted).
 */
HashMemory *del_HashMemory(HashMemory *mem, int force)
{
  const char *caller = "del_HashMemory";
  if(mem) {
    if(!force && (busy_FreeListNodes(mem->hash_memory) > 0 ||
		  busy_FreeListNodes(mem->node_memory) > 0)) {
      lprintf(stderr, "%s: Free-list in use.\n", caller);
      return NULL;
    };
    mem->hash_memory = del_FreeList(caller, mem->hash_memory, force);
    mem->node_memory = del_FreeList(caller, mem->node_memory, force);
    mem->string_memory = del_StringMem(caller, mem->string_memory, force);
    free(mem);
  };
  return NULL;
}

/*.......................................................................
 * Display the number of active hash-tables and hash nodes that are 
 * currently allocated from a given freelist
 *
 * Input:
 *  mem    HashMemory *  The freelist to describe.
 */
void show_HashMemory(HashMemory *mem)
{
  if(!mem) {
    lprintf(stderr, "show_HashMemory: NULL argument.\n");
    return;
  };
  lprintf(stdout,
       "There are currently %ld hash nodes allocated to %ld hash-table(s).\n",
	  busy_FreeListNodes(mem->node_memory),
	  busy_FreeListNodes(mem->hash_memory));
}

/*.......................................................................
 * Create a new hash table.
 *
 * Input:
 *  mem       HashMemory *  An optional free-list for use in allocating
 *                          HashTable containers and nodes. See explanation
 *                          in hash.h. If you are going to allocate more
 *                          than one hash table, then it will be more
 *                          efficient to allocate a single free-list for
 *                          all of them than to force each hash table
 *                          to allocate its own private free-list.
 *  size             int    The size of the hash table. Best performance
 *                          will be acheived if this is a prime number.
 *  hcase       HashCase    Specify how symbol case is considered when
 *                          looking up symbols, from:
 *                           IGNORE_CASE - Upper and lower case versions
 *                                         of a letter are treated as
 *                                         being identical.
 *                           HONOUR_CASE - Upper and lower case versions
 *                                         of a letter are treated as
 *                                         being distinct.
 *                          characters in a lookup name is significant.
 *  app_data        void *  Optional application data to be registered
 *                          to the table. This is presented to user
 *                          provided SYM_DEL_FN() symbol destructors along
 *                          with the symbol data.
 *  del_fn() HASH_DEL_FN(*) If you want app_data to be free'd when the
 *                          hash-table is destroyed, register a suitable
 *                          destructor function here.
 * Output:
 *  return HashTable *  The new hash table, or NULL on error.
 */
HashTable *new_HashTable(HashMemory *mem, int size, HashCase hcase,
			 void *app_data, HASH_DEL_FN(*del_fn))
{
  HashTable *hash;         /* The table to be returned */
  int allocate_mem = !mem; /* True if mem should be internally allocated */
  int i;
/*
 * Check arguments.
 */
  if(size <= 0) {
    lprintf(stderr, "new_HashTable: Illegal table size (%d).\n", size);
    return NULL;
  };
/*
 * Allocate an internal free-list?
 */
  if(allocate_mem) {
    mem = new_HashMemory(1, 100);
    if(!mem)
      return NULL;
  };
/*
 * Allocate the container.
 */
  hash = (HashTable* )new_FreeListNode("new_HashTable", mem->hash_memory);
  if(!hash) {
    if(allocate_mem)
      mem = del_HashMemory(mem, 1);
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize
 * the container at least up to the point at which it can safely
 * be passed to del_HashTable().
 */
  hash->ref = 0;
  hash->mem = mem;
  hash->internal_mem = allocate_mem;
  hash->case_sensitive = hcase==HONOUR_CASE;
  hash->size = size;
  hash->bucket = NULL;
  hash->keycmp = hash->case_sensitive ? strcmp : lower_strcmp;
  hash->app_data = app_data;
  hash->del_fn = del_fn;
/*
 * Allocate the array of 'size' hash buckets.
 */
  hash->bucket = (HashBucket *) malloc(sizeof(HashBucket) * size);
  if(!hash->bucket) {
    lprintf(stderr, "new_HashTable: Insufficient memory for %d buckets.\n",
	    size);
    return del_HashTable(hash);
  };
/*
 * Initialize the bucket array.
 */
  for(i=0; i<size; i++) {
    HashBucket *b = hash->bucket + i;
    b->head = NULL;
    b->count = 0;
  };
/*
 * The table is ready for use - albeit currently empty.
 */
  return hash;
}

/*.......................................................................
 * Increment the reference count of a hash-table to prevent deletion while
 * it is in use. Note that the reference count starts as zero.
 *
 * Input:
 *  hash   HashTable *  The hash table to be newly referenced.
 * Output:
 *  return HashTable *  The same as 'hash'.
 */
HashTable *ref_HashTable(HashTable *hash)
{
  if(hash)
    hash->ref++;
  return hash;
}

/*.......................................................................
 * Delete a reference to a hash-table. If the hash-table's reference count
 * is non-zero then the reference count will be decremented. Only when the
 * reference count reaches zero will the hash-table actually be deleted.
 *
 * Input:
 *  hash   HashTable *  The hash table to be deleted.
 * Output:
 *  return HashTable *  The deleted hash table (always NULL).
 */
HashTable *del_HashTable(HashTable *hash)
{
  if(hash && (hash->ref==0 || --hash->ref==0)) {
    
    // Clear and delete the bucket array.

    if(hash->bucket) {
      clear_HashTable(hash);
      free(hash->bucket);
      hash->bucket = NULL;
    };
    
    // Delete application data.

    if(hash->del_fn)
      hash->del_fn(hash->app_data);
    
    // If the hash table was allocated from an internal free-list,
    // delete it and the hash table by deleting the
    // free-list. Otherwise just return the hash-table to the external
    // free-list.

    if(hash->internal_mem)
      del_HashMemory(hash->mem, 1);
    else
      hash = (HashTable* )del_FreeListNode("del_HashTable", hash->mem->hash_memory,
					   hash);
  };
  return NULL;
}

/*.......................................................................
 * Create and install a new entry in a hash table.
 *
 * Input:
 *  hash   HashTable *  The hash table to insert the symbol into.
 *  name        char *  The name to tag the entry with.
 *  code         int    An application-specific code to be stored in
 *                      the entry.
 *  fn  void (*)(void)  An application-specific function to be stored
 *                      in the entry.
 *  data        void *  An application-specific pointer to data to be
 *                      associated with the entry, or NULL if not
 *                      relevant.
 *  del_fn SYM_DEL_FN(*) An optional destructor function. When the
 *                      symbol is deleted this function will be called
 *                      with the 'code' and 'data' arguments given
 *                      above. Any application data that was registered
 *                      to the table via the app_data argument of
 *                      new_HashTable() will also be passed.
 * Output:
 *  return  HashNode *  The new entry, or NULL on error.
 */
Symbol *new_HashSymbol(HashTable *hash, char *name, int code, void (*fn)(void),
		       void *data, SYM_DEL_FN(*del_fn))
{
  HashBucket *bucket;  /* The hash-bucket associated with the name */
  HashNode *node;      /* The new node */
/*
 * Check arguments.
 */
  if(!hash || !name) {
    lprintf(stderr, "new_HashSymbol: NULL %s argument.\n",
	    !hash ? "hash" : "name");
    return NULL;
  };
/*
 * Get the hash bucket of the specified name.
 */
  bucket = find_HashBucket(hash, name);
/*
 * See if a node with the same name already exists.
 */
  if(find_HashNode(hash, bucket, name, NULL)) {
    lprintf(stderr, "new_HashSymbol: Symbol \"%s\" already exists.\n",
	    name);
    return NULL;
  };
/*
 * Allocate a new node.
 */
  node = new_HashNode(hash, name, code, fn, data, del_fn);
  if(!node)
    return NULL;
/*
 * Install the node at the head of the hash-bucket list.
 */
  node->next = bucket->head;
  bucket->head = node;
  bucket->count++;
  return &node->symbol;
}

/*.......................................................................
 * Remove and delete a given hash-table entry.
 *
 * Input:
 *  hash   HashTable *  The hash table to find the symbol in.
 *  name        char *  The name of the entry.
 * Output:
 *  return  HashNode *  The deleted hash node (always NULL).
 */
Symbol *del_HashSymbol(HashTable *hash, char *name)
{
  if(hash && name) {
    HashBucket *bucket = find_HashBucket(hash, name);
    HashNode *prev;   /* The node preceding the located node */
    HashNode *node = find_HashNode(hash, bucket, name, &prev);
/*
 * Node found?
 */
    if(node) {
/*
 * Remove the node from the bucket list.
 */
      if(prev) {
	prev->next = node->next;
      } else {
	bucket->head = node->next;
      };
/*
 * Record the loss of a node.
 */
      bucket->count--;
/*
 * Delete the node.
 */
      (void) del_HashNode(hash, node);
    };
  };
  return NULL;
}

/*.......................................................................
 * Look up a symbol in the hash table.
 *
 * Input:
 *  hash   HashTable *   The table to look up the string in.
 *  name        char *   The name of the symbol to look up.
 * Output:
 *  return    Symbol *   The located hash-table symbol, or NULL if not
 *                       found.
 */
Symbol *find_HashSymbol(HashTable *hash, char *name)
{
  HashBucket *bucket;  /* The hash-table bucket associated with name[] */
  HashNode *node;      /* The hash-table node of the requested symbol */
/*
 * Check arguments.
 */
  if(!hash || !name) {
    lprintf(stderr, "find_HashSymbol: NULL %s argument.\n",
	    !hash ? "hash" : "name");
    return NULL;
  };
/*
 * Hash the name to a hash-table bucket.
 */
  bucket = find_HashBucket(hash, name);
/*
 * Find the bucket entry that exactly matches the name.
 */
  node = find_HashNode(hash, bucket, name, NULL);
  if(!node)
    return NULL;
  return &node->symbol;
}

/*.......................................................................
 * Private function used to allocate a hash-table node.
 * The caller is responsible for checking that the specified symbol
 * is unique and for installing the returned entry in the table.
 *
 * Input:
 *  hash     HashTable *  The table to allocate the node for.
 *  name          char *  The name of the new entry.
 *  code           int    A user-supplied context code.
 *  fn  void (*)(void)    A user-supplied function pointer.
 *  data          char *  A user-supplied data pointer.
 *  del_fn  SYM_DEL_FN(*) An optional 'data' destructor function.
 * Output:
 *  return    HashNode *  The new node, or NULL on error.
 */
static HashNode *new_HashNode(HashTable *hash, char *name, int code,
			      void (*fn)(void), void *data, SYM_DEL_FN(*del_fn))
{
  HashNode *node;  /* The new node */
/*
 * Allocate the new node from the free list.
 */
  node = (HashNode* )new_FreeListNode("new_HashNode", hash->mem->node_memory);
  if(!node)
    return NULL;
/*
 * Before attempting any operation that might fail, initialize the
 * contents of 'node' at least up to the point at which it can be
 * safely passed to del_HashNode().
 */
  node->symbol.name = NULL;
  node->symbol.code = code;
  node->symbol.fn = fn;
  node->symbol.data = data;
  node->symbol.del_fn = del_fn;
  node->next = NULL;
/*
 * Allocate a copy of 'name'.
 */
  node->symbol.name = new_StringMemString("new_HashNode",
					  hash->mem->string_memory,
					  strlen(name) + 1);
  if(!node->symbol.name) 
    return del_HashNode(hash, node);
/*
 * If character-case is insignificant in the current table, convert the
 * name to lower case while copying it.
 */
  if(hash->case_sensitive) {
    strcpy(node->symbol.name, name);
  } else {
    char *src = name;
    char *dst = node->symbol.name;
    for( ; *src; src++,dst++)
      *dst = tolower(*src);
    *dst = '\0';
  };
  return node;
}

/*.......................................................................
 * Private function used to delete a hash-table node.
 * The node must have been removed from its list before calling this
 * function.
 *
 * Input:
 *  hash   HashTable *  The table for which the node was originally
 *                      allocated.
 *  node    HashNode *  The node to be deleted.
 * Output:
 *  return  HashNode *  The deleted node (always NULL).
 */
static HashNode *del_HashNode(HashTable *hash, HashNode *node)
{
  if(node) {
    node->symbol.name = del_StringMemString("del_HashNode",
					    hash->mem->string_memory,
					    node->symbol.name);
/*
 * Call the user-supplied data-destructor if provided.
 */
    if(node->symbol.data && node->symbol.del_fn)
      node->symbol.data = node->symbol.del_fn(hash->app_data,
					      node->symbol.code,
					      node->symbol.data);
/*
 * Return the node to the free-list.
 */
    node->next = NULL;
    node = (HashNode* )del_FreeListNode("del_HashNode", hash->mem->node_memory, 
					node);
  };
  return NULL;
}

/*.......................................................................
 * Private function to locate the hash bucket associated with a given
 * name.
 *
 * This uses a hash-function described in the dragon-book
 * ("Compilers - Principles, Techniques and Tools", by Aho, Sethi and
 *  Ullman; pub. Adison Wesley) page 435.
 *
 * Input:
 *  hash    HashTable *   The table to look up the string in.
 *  name         char *   The name of the symbol to look up.
 * Output:
 *  return HashBucket *   The located hash-bucket.
 */
static HashBucket *find_HashBucket(HashTable *hash, char *name)
{
  unsigned char *kp;
  unsigned long h = 0L;
  if(hash->case_sensitive) {
    for(kp=(unsigned char *) name; *kp; kp++)
      h = 65599UL * h + *kp;  /* 65599 is a prime close to 2^16 */
  } else {
    for(kp=(unsigned char *) name; *kp; kp++)
      h = 65599UL * h + tolower((int)*kp);  /* 65599 is a prime close to 2^16 */
  };
  return hash->bucket + (h % hash->size);
}

/*.......................................................................
 * Search for a given name in the entries of a given bucket.
 *
 * Input:
 *  hash     HashTable *  The hash-table being searched.
 *  bucket  HashBucket *  The bucket to search (use find_HashBucket()).
 *  name          char *  The name to search for.
 * Output:
 *  prev      HashNode ** If prev!=NULL then the pointer to the node
 *                        preceding the located node in the list will
 *                        be recorded in *prev. This will be NULL either
 *                        if the name is not found or the located node is
 *                        at the head of the list of entries.
 * return     HashNode *  The located hash-table node, or NULL if not
 *                        found.
 */
static HashNode *find_HashNode(HashTable *hash, HashBucket *bucket,
			       char *name, HashNode **prev)
{
  HashNode *last;  /* The previously searched node */
  HashNode *node;  /* The node that is being searched */
/*
 * Search the list for a node containing the specified name.
 */
  for(last=NULL, node=bucket->head;
      node && hash->keycmp(node->symbol.name, name)!=0;
      last = node, node=node->next)
    ;
  if(prev)
    *prev = node ? last : NULL;
  return node;
}

/*.......................................................................
 * When hash->case_sensitive is zero this function is called
 * in place of strcmp(). In such cases the hash-table names are stored
 * as lower-case versions of the original strings so this function
 * performs the comparison against lower-case copies of the characters
 * of the string being compared.
 *
 * Input:
 *  node_key   char *  The lower-case hash-node key being compared against.
 *  look_key   char *  The lookup key.
 * Output:
 *  return      int    <0 if node_key < look_key.
 *                      0 if node_key == look_key.
 *                     >0 if node_key > look_key.
 */
static int lower_strcmp(const char *node_key, const char *look_key)
{
  int cn;  /* The latest character from node_key[] */
  int cl;  /* The latest character from look_key[] */
  do {
    cn = *node_key++;
    cl = *look_key++;
  } while(cn && cn==tolower(cl));
  return cn - tolower(cl);
}

/*.......................................................................
 * Display the contents of a hash table to the standard output.
 *
 * Input:
 *  hash      HashTable *  The hash table to be displayed.
 *  summarize       int    If non-zero then occupancy statistics will
 *                         be displayed instead of the actual symbol
 *                         contents.
 */
void show_HashTable(HashTable *hash, int summarize)
{
  int i;
  if(summarize) {
    int total = 0;  /* The total number of symbols in the table */
/*
 * Count the total number of symbols and the average occupancy per
 * bucket.
 */
    for(i=0; i<hash->size; i++)
      total += hash->bucket[i].count;
/*
 * Display the results.
 */
    lprintf(stdout,
      "\nThe hash table contains %d symbols in %d buckets (%.3g per bucket).\n",
      total, hash->size, (float)total/hash->size);
  } else {
/*
 * Display the symbol contents of the hash table one bucket at a time.
 */
    for(i=0; i<hash->size; i++) {
      HashBucket *bucket = hash->bucket + i;
      HashNode *node;
      lprintf(stdout, "\nHash bucket %d contains %d entries.\n", i,
	      bucket->count); 
/*
 * Show each symbol of the current bucket.
 */
      for(node=bucket->head; node; node=node->next) {
	Symbol *sym = &node->symbol;
	printf(" name=%s code=%d data=%p\n", sym->name, sym->code, sym->data);
      };
    };
  };
  return;
}

/*.......................................................................
 * Empty a hash-table by deleting all of its entries.
 *
 * Input:
 *  hash    HashTable *  The hash table to clear.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
int clear_HashTable(HashTable *hash)
{
  int i;
  if(!hash) {
    lprintf(stderr, "clear_HashTable: NULL argument.\n");
    return 1;
  };
/*
 * Clear the contents of the bucket array.
 */
  for(i=0; i<hash->size; i++) {
    HashBucket *bucket = hash->bucket + i;
/*
 * Delete the list of active hash nodes from the bucket.
 */
    HashNode *node = bucket->head;
    while(node) {
      HashNode *next = node->next;
      (void) del_HashNode(hash, node);
      node = next;
    };
/*
 * Mark the bucket as empty.
 */
    bucket->head = NULL;
    bucket->count = 0;
  };
  return 0;
}

/*.......................................................................
 * Execute a given function on each entry of a hash table, returning
 * before completion if the the specified function returns non-zero.
 *
 * Input:
 *  hash       HashTable *    The table to traverse.
 *  scan_fn HASH_SCAN_FN(*)   The function to call.
 *  context         void *    Optional caller-specific context data
 *                            to be passed to scan_fn().
 * Output:
 *  return           int      0 - OK.
 *                            1 - Either the arguments were invalid, or
 *                                scan_fn() returned non-zero at some
 *                                point.
 */
int scan_HashTable(HashTable *hash, HASH_SCAN_FN(*scan_fn), void *context)
{
  int i;
/*
 * Check the arguments.
 */
  if(!hash || !scan_fn) {
    lprintf(stderr, "scan_HashTable: NULL argument(s).\n");
    return 1;
  };
/*
 * Iterate through the buckets of the table.
 */
  for(i=0; i<hash->size; i++) {
    HashBucket *bucket = hash->bucket + i;
    HashNode *node;
/*
 * Iterate through the list of symbols that fall into bucket i,
 * passing each one to the caller-specified function.
 */
    for(node=bucket->head; node; node=node->next) {
      if(scan_fn(&node->symbol, context))
	return 1;
    };
  };
  return 0;
}
