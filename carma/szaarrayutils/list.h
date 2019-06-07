#ifndef list_h
#define list_h

/*
 * List containers and nodes are small objects. Separately allocating
 * many such objects would normally cause memory fragmentation. To
 * counter this, ListMemory objects are used. These contain
 * dedicated free-lists formed from large dynamically allocated arrays
 * of objects. One ListMemory object can be shared between multiple lists
 * (within a single thread).
 */
typedef struct ListMemory ListMemory;

/* Create a free-list for allocation of lists */

ListMemory *new_ListMemory(int list_count, int node_count);

/*
 * Display the number of active lists and list nodes that are 
 * currently allocated from a given freelist
 */
void show_ListMemory(ListMemory *mem);

/*
 * Return all allocated lists and their contents to the freelist.
 * This function should not be called unless if it is known that none
 * of the allocated lists are still in use.
 */

void rst_ListMemory(ListMemory *mem);

/* Delete a redundant free-list if not being used */

ListMemory *del_ListMemory(ListMemory *mem, int force);

/*
 * Declare the contents of a generic list node.
 */
typedef struct ListNode ListNode;
struct ListNode {
  void *data;           /* Application supplied data */
  ListNode *next;       /* The next node in the list, or NULL */
};

/*
 * Declare a list container.
 */
typedef struct {
  unsigned ref;         /* Reference count */
  unsigned nnode;       /* The number of nodes in the list */
  ListMemory *mem;      /* Memory allocation container */
  int internal_mem;     /* True if mem was allocated by new_List() */
  ListNode *head;       /* The first node of the list */
  ListNode *tail;       /* The last node of the list */
} List;

/* Create a new list using memory from a given ListMemory container */

List *new_List(ListMemory *mem);

/* Delete a reference to a list */

List *del_List(List *list);

/* Increment the reference count of a list to prevent untimely deletion */

List *ref_List(List *list);

/* Discard the contents of a list via calls to del_ListNode() */

List *clr_List(List *list);

/* Append a node to the end of a list */

ListNode *append_ListNode(List *list, void *data);

/* Prepend a node to the head of a list */

ListNode *prepend_ListNode(List *list, void *data);

/* Insert a node into a list after a given node */

ListNode *insert_ListNode(List *list, ListNode *prev, void *data);

/*
 * Delete a list node container and return its 'data' pointer.
 * The node can be identified either by its node pointer or its data pointer.
 */

void *del_ListNode(List *list, ListNode *node, void *data);

/* Find the first list node that contains a given data pointer */

ListNode *find_ListNode(List *list, void *data);

#endif
