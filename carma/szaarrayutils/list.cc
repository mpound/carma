#include <stdio.h>
#include <stdlib.h>

#include "carma/szaarrayutils/list.h"
#include "carma/szaarrayutils/freelist.h"
#include "carma/szaarrayutils/lprintf.h"

/*
 * The following container object contains free-lists to be used
 * for allocation of list containers and nodes.
 */
struct ListMemory {
  FreeList *list_memory;    /* Memory used for allocating List containers */
  FreeList *node_memory;    /* Memory used for allocating ListNode nodes */
};

static ListNode *new_ListNode(List *list, void *data);

/*.......................................................................
 * Allocate a free-list for use in allocating lists and their nodes.
 *
 * Input:
 *  list_count    int    The number of List containers per free-list
 *                       block.
 *  node_count    int    The number of List nodes per free-list block.
 * Output:
 *  return ListMemory *  The new free-list for use in allocating lists
 *                       and their nodes.
 */
ListMemory *new_ListMemory(int list_count, int node_count)
{
  ListMemory *mem;
/*
 * Allocate the free-list container.
 */
  mem = (ListMemory *) malloc(sizeof(ListMemory));
  if(!mem) {
    lprintf(stderr, "new_ListMemory: Insufficient memory.\n");
    return NULL;
  };
/*
 * Initialize the container at least up to the point at which it can
 * safely be passed to del_ListMemory().
 */
  mem->list_memory = NULL;
  mem->node_memory = NULL;
/*
 * Allocate the two free-lists.
 */
  mem->list_memory = new_FreeList("new_ListMemory", sizeof(List), list_count);
  if(!mem->list_memory)
    return del_ListMemory(mem, 1);
  mem->node_memory = new_FreeList("new_ListMemory", sizeof(ListNode),
				  node_count);
  if(!mem->node_memory)
    return del_ListMemory(mem, 1);
/*
 * Return the free-list container.
 */
  return mem;
}

/*.......................................................................
 * Return all allocated lists and their contents to the freelist.
 * This function should not be called unless if it is known that none
 * of the currently allocated lists are still being used.
 *
 * Input:
 *  mem    ListMemory *   The freelist to be reset.
 */
void rst_ListMemory(ListMemory *mem)
{
  if(mem) {
    rst_FreeList(mem->list_memory);
    rst_FreeList(mem->node_memory);
  };
}

/*.......................................................................
 * Delete a List free-list. An error will be displayed if the list is
 * still in use and the deletion will be aborted.
 *
 * Input:
 *  mem    ListMemory *  The free-list container to be deleted.
 *  force         int    If force==0 then del_ListMemory() will complain
 *                        and refuse to delete the free-list if any
 *                        of nodes have not been returned to the free-list.
 *                       If force!=0 then del_ListMemory() will not check
 *                        whether any nodes are still in use and will
 *                        always delete the list.
 * Output:
 *  return ListMemory *  Always NULL (even if the memory couldn't be
 *                       deleted).
 */
ListMemory *del_ListMemory(ListMemory *mem, int force)
{
  if(mem) {
    if(!force && (busy_FreeListNodes(mem->list_memory) > 0 ||
		  busy_FreeListNodes(mem->node_memory) > 0)) {
      lprintf(stderr, "del_ListMemory: Free-list in use.\n");
      return NULL;
    };
    mem->list_memory = del_FreeList("del_ListMemory", mem->list_memory, force);
    mem->node_memory = del_FreeList("del_ListMemory", mem->node_memory, force);
    free(mem);
  };
  return NULL;
}

/*.......................................................................
 * Display the number of active lists and list nodes that are 
 * currently allocated from a given freelist
 *
 * Input:
 *  mem    ListMemory *  The freelist to describe.
 */
void show_ListMemory(ListMemory *mem)
{
  if(!mem) {
    lprintf(stderr, "show_ListMemory: NULL argument.\n");
    return;
  };
  lprintf(stdout,
	  "There are currently %ld list nodes allocated to %ld lists.\n",
	  busy_FreeListNodes(mem->node_memory),
	  busy_FreeListNodes(mem->list_memory));
}

/*.......................................................................
 * Create a new list using memory from a given ListMemory container.
 *
 * Input:
 *  mem    ListMemory *   An optional free-list to allocate the list
 *                        and its nodes from. Do not delete mem while
 *                        this list is in use. If you are creating a
 *                        lot of lists it is more efficient to allocate
 *                        them and their nodes from a single free-list,
 *                        than for each list to have to allocate its own
 *                        free-list. 
 * Output:
 *  return       List *   A new empty list or NULL on error.
 */
List *new_List(ListMemory *mem)
{
  List *list;              /* The new list */
  int allocate_mem = !mem; /* True if mem should be internally allocated */
/*
 * Allocate an internal free-list?
 */
  if(allocate_mem) {
    mem = new_ListMemory(1, 100);
    if(!mem)
      return NULL;
  };
/*
 * Allocate the container.
 */
  list = (List* )new_FreeListNode("new_List", mem->list_memory);
  if(!list) {
    if(allocate_mem)
      mem = del_ListMemory(mem, 1);
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * del_List().
 */
  list->ref = 0;
  list->nnode = 0;
  list->mem = mem;
  list->internal_mem = allocate_mem;
  list->head = NULL;
  list->tail = NULL;
  return list;
}

/*.......................................................................
 * Increment the reference count of a list to prevent deletion while the
 * list is in use. Note that the reference count starts as zero.
 *
 * Input:
 *  list     List *   The list to be newly referenced.
 * Output:
 *  return   List *   The same as 'list'.
 */
List *ref_List(List *list)
{
  if(list)
    list->ref++;
  return list;
}

/*.......................................................................
 * Discard the contents of a list via calls to del_ListNode().
 *
 * Input:
 *  list       List *  The list to be cleared.
 * Output:
 *  return     List *  The same as 'list', or NULL on error.
 */
List *clr_List(List *list)
{
  if(list) {
    while(list->head)
      del_ListNode(list, list->head, NULL);
    list->nnode = 0;
  };
  return list;
}

/*.......................................................................
 * Delete a reference to a list. If the list's reference count is non-zero
 * then the reference count will be decremented. Only when the reference
 * count reaches zero will the list actually be deleted.
 *
 * Input:
 *  list     List *   The list to be deleted.
 * Output:
 *  return   List *   Always NULL.
 */
List *del_List(List *list)
{
  if(list && (list->ref==0 || --list->ref==0)) {
/*
 * If the list memory freelist was allocated internally then simply
 * delete it. This will delete the list that contains it as well as
 * itself and all the list nodes.
 */
    if(list->internal_mem) {
      del_ListMemory(list->mem, 1);
    } else {
/*
 * Discard all list nodes.
 */
      clr_List(list);
/*
 * Return the list container to the free list.
 */
      list = (List* )del_FreeListNode("del_List", list->mem->list_memory, list);
    };
  };
  return NULL;
}

/*.......................................................................
 * Create a new list node to be added somewhere within a given list.
 *
 * Input:
 *  list       List *  The list that is destined to contain the node.
 *  data       void *  The required contents of the list node.
 * Output:
 *  return ListNode *  The new list node or NULL on error.
 */
static ListNode *new_ListNode(List *list, void *data)
{
  ListNode *node;
  if(!list) {
    lprintf(stderr, "new_ListNode: NULL list.\n");
    return NULL;
  };
/*
 * Allocate the new node from the free list.
 */
  node = (ListNode* )new_FreeListNode("new_ListNode", list->mem->node_memory);
  if(!node)
    return NULL;
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * del_ListNode().
 */
  node->data = data;
  node->next = NULL;
/*
 * Increment the cound of list nodes.
 */
  list->nnode++;
  return node;
}

/*.......................................................................
 * Append a node to the end of a list.
 *
 * Input:
 *  list        List *  The list to append to.
 *  data        void *  Data to record in the new node.
 * Output:
 *  return  ListNode *  The new list node, or NULL on error.
 */
ListNode *append_ListNode(List *list, void *data)
{
  ListNode *node = new_ListNode(list, data);
  if(node) {
    if(!list->head) {
      list->head = list->tail = node;
    } else {
      list->tail->next = node;
      list->tail = node;
    };
  };
  return node;
}

/*.......................................................................
 * Prepend a node to the start of a list.
 *
 * Input:
 *  list        List *  The list to prepend to.
 *  data        void *  Data to record in the new node.
 * Output:
 *  return  ListNode *  The new list node, or NULL on error.
 */
ListNode *prepend_ListNode(List *list, void *data)
{
  ListNode *node = new_ListNode(list, data);
  if(node) {
    if(!list->head) {
      list->head = list->tail = node;
    } else {
      node->next = list->head;
      list->head = node;
    };
  };
  return node;
}

/*.......................................................................
 * Insert a new node after a given node in a list.
 *
 * Input:
 *  list        List *  The list to insert the node into.
 *  prev    ListNode *  The preceding list node, or NULL for the head
 *                      of the list.
 *  data        void *  Data to record in the new node.
 * Output:
 *  return  ListNode *  The new list node, or NULL on error.
 */
ListNode *insert_ListNode(List *list, ListNode *prev, void *data)
{
  ListNode *node;  /* The new node */
/*
 * Insert at the head of the list?
 */
  if(!prev)
    node = prepend_ListNode(list, data);
/*
 * Insert at the tail of the list.
 */
  else if(prev == list->tail)
    node = append_ListNode(list, data);
/*
 * Insert between two existing nodes.
 */
  else {
    node = new_ListNode(list, data);
    if(node) {
      node->next = prev->next;
      prev->next = node;
    };
  };
  return node;
}

/*.......................................................................
 * Delete a list node container and return its 'data' pointer.
 * The container can be identified either by its ListNode pointer or
 * its ListNode::data pointer.
 *
 * Input:
 *  list      List *  The list that contains the node.
 *  node  ListNode *  The node to be deleted (or NULL).
 *  data      void *  If node==NULL then the first node that has
 *                    node->data == data, will be deleted.
 * Output:
 *  return    void *  The data of the list node, or NULL on error or
 *                    if node==NULL.
 */
void *del_ListNode(List *list, ListNode *node, void *data)
{
/*
 * NULL operation?
 */
  if(!node && !data)
    return NULL;
/*
 * Bad inputs?
 */
  if(!list || !list->head) {
    lprintf(stderr, "del_ListNode: Empty list.\n");
    return NULL;
  };
/*
 * Delete the node at the head of the list?
 */
  if(!node && data==list->head->data)
    node = list->head;
  if(node==list->head) {
    if(node==list->tail)
      list->head = list->tail = NULL;
    else
      list->head = node->next;
/*
 * Search for the node that precedes the specified node.
 */
  } else {
    ListNode *prev;  /* The node preceding the node being checked */
/*
 * Locate the node to be deleted.
 */
    if(node) {
      for(prev=list->head; prev->next!=node; prev=prev->next)
	;
    } else {
      prev = list->head;
      for(node=prev->next; node && node->data!=data; prev=node,node=node->next)
	;
    };
    if(!prev->next) {
      lprintf(stderr, "del_ListNode: Node not found.\n");
      return NULL;
    };
/*
 * Delete the node at the end of the list?
 */
    if(node==list->tail) {
      prev->next = NULL;
      list->tail = prev;
/*
 * Delete from between two nodes?
 */
    } else {
      prev->next = node->next;
    };
  };
/*
 * Record the data of the node for return.
 */
  data = node->data;
/*
 * Return the node to the free-list.
 */
  node->next = NULL;
  node->data = NULL;
  node = (ListNode*)del_FreeListNode("del_ListNode", list->mem->node_memory, node);
/*
 * Decrement the count of list nodes.
 */
  list->nnode--;
/*
 * Return the data of the node.
 */
  return data;
}

/*.......................................................................
 * Return the first list node that contains a given data pointer.
 *
 * Input:
 *  list       List *  The list to be searched.
 *  data       void *  The data pointer to be sought. This should be the
 *                     pointer that was originally supplied to
 *                     append_ListNode(), insert_ListNode(), or
 *                     prepend_ListNode().
 * Output:
 *  return ListNode *  The first list node with node->data == data.
 */
ListNode *find_ListNode(List *list, void *data)
{
  ListNode *node;   /* The node being searched */
  if(!list) {
    lprintf(stderr, "find_ListNode: NULL argument.\n");
    return NULL;
  };
  for(node = list->head; node && node->data != data; node=node->next)
    ;
  return node;
}

