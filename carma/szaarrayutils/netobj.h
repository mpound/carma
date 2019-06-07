#ifndef netobj_h
#define netobj_h

#include <stddef.h>  /* size_t */

#include "carma/szaarrayutils/netbuf.h"

typedef struct NetObjTable NetObjTable;

/*
 * This file describes network-transparent objects and the functions
 * that manipulate them.
 */

int obj_to_net(const NetObjTable *types, sza::array::NetBuf *net, int id, void *obj);
int net_to_obj(const NetObjTable *types, sza::array::NetBuf *net, int id, void *obj);

/* Enumerate known basic database types */

typedef enum {
  NET_ASCII,   /* char elements interpretted as a '\0' terminated ASCII string*/
  NET_BYTE,    /* char elements interpretted as 8-bit integers */
  NET_SHORT,   /* short int elements */
  NET_ENUM,    /* short int enumeration (Use type NetEnum) */
  NET_BOOL,    /* short int boolean (0:false, !=0:true) (use type NetBool) */
  NET_MASK,    /* unsigned long bit-mask (Use type NetMask) */
  NET_LONG,    /* long int elements */
  NET_FLOAT,   /* float elements */
  NET_DOUBLE   /* double elements */
} NetDataType;

/* Declare type aliases for special types */

typedef short NetEnum;
typedef short NetBool;
typedef unsigned long NetMask;
typedef unsigned long NetUlong;

/* Declare a symbol table for enumeration members */

typedef struct {
  char *name;     /* Symbolic name for the enumerator */
} NetObjEnumTab;

/* Declare a type to be used to describe one member of an object */

typedef struct {
  const char *name; /* Member name */
  size_t offset;    /* Byte offset of member wrt start of object */
  NetDataType type; /* The data-type of the member */
  int ntype;        /* The number of elements of type 'type' in the member */
  const NetObjEnumTab *etab; /* Enumeration symbols (NULL if type!=NET_ENUM) */
  int nenum;        /* The number of elements in etab[] (0 if type!=NET_ENUM) */
} NetObjMember;

/* Declare an object description type */

typedef struct {
  char *name;             /* Name to refer to object by */
  const NetObjMember *member;/* An array of nmember object member descriptors */
  int nmember;            /* The number of elements in member[] */
  size_t native_size;     /* The native size of the object */
} NetObjInfo;

/*
 * A container of the following type should be used to describe each
 * of the network objects that are to be supported.
 */
struct NetObjTable {
  char *name;                /* The name of the object collection */
  const NetObjInfo *info;    /* Descriptions of each of nobj objects */
                             /* This can be NULL if nobj==0. */
  int nobj;                  /* The number of objects in the database */
  size_t union_size;         /* The size of the union of all object types */
};

size_t net_max_obj_size(const NetObjTable *types);
size_t net_obj_size(const NetObjTable *types, int id);
size_t net_type_size(NetDataType type);
size_t net_native_size(const NetObjTable *types, int id);
const NetObjInfo *net_obj_info(const NetObjTable *types, int id);

#endif
