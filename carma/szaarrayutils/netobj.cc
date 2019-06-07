#include <string.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/netobj.h"

using namespace sza::array;

/*.......................................................................
 * Pack a network object into a network buffer for transmission.
 *
 * Input:
 *  types  NetObjTable *   A table of object definitions.
 *  net         NetBuf *   The network buffer in which to pack the object.
 *                         Note that the caller is responsible for calling
 *                         net_start_put() and net_end_put().
 *  id             int     The index of the object definition in 'types'.
 *  obj           void *   The object to be packed. This can be NULL if the
 *                         object description in the corresponding entry
 *                         of 'types' contains no members.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int obj_to_net(const NetObjTable *types, NetBuf *net, int id, void *obj)
{
  const NetObjInfo *info;   /* The definition of the object being packed */
  int i;
  
  // Check arguments.

  if(!types || !net) {
    lprintf(stderr, "obj_to_net: NULL %s argument.\n", !types ? "types":"net");
    return 1;
  };
  
  // Look up the object definition.

  info = net_obj_info(types, id);
  if(!info)
    return 1;

  
  // A NULL object is legal if the object contains no members.

  if(!obj && info->nmember != 0) {
    lprintf(stderr, "obj_to_net: NULL '%s' object.\n", info->name);
    return 1;
  };
  
  // Pack the members of the network object sequentially.

  for(i=0; i<info->nmember; i++) {
    const NetObjMember *member = info->member + i;
    
    // Get a pointer to the start of the member in the object.

    void *data = (char *)obj + member->offset;
    
    // Write the member according to its type.

    switch(member->type) {
    case NET_ASCII:
      {
	unsigned long len = strlen((const char* )data);
	if(net_put_long(net, 1, &len) || 
	   net_put_char(net, len, (unsigned char* )data))
	  return 1;
      };
      break;
    case NET_BYTE:
      if(net_put_char(net, member->ntype, (unsigned char* )data))
	return 1;
      break;
    case NET_SHORT:
    case NET_ENUM:
    case NET_BOOL:
      if(net_put_short(net, member->ntype, (unsigned short* )data))
	return 1;
      break;
    case NET_MASK:
    case NET_LONG:
      if(net_put_long(net, member->ntype, (unsigned long* )data))
	return 1;
      break;
    case NET_FLOAT:
      if(net_put_float(net, member->ntype, (float* )data))
	return 1;
      break;
    case NET_DOUBLE:
      if(net_put_double(net, member->ntype, (double* )data))
	return 1;
      break;
    default:
      lprintf(stderr, "obj_to_net: %s.%s has an unknown data-type (%d).\n",
	      info->name, member->name, (int) member->type);
      return 1;
      break;
    };
  };
  return 0;
}

/*.......................................................................
 * Un-pack a network object from a network buffer.
 *
 * Input:
 *  types  NetObjTable *   A table of object definitions.
 *  net         NetBuf *   The network buffer containing the packed object.
 *                         Note that the caller is responsible for calling
 *                         net_start_get() and net_end_get().
 *  id             int     The index of the object definition in 'types'.
 * Input/Output:
 *  obj           void *   The output object. This can be NULL if the
 *                         object description in the corresponding entry
 *                         of 'types' contains no members.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int net_to_obj(const NetObjTable *types, NetBuf *net, int id, void *obj)
{
  const NetObjInfo *info;   /* The definition of the object being un-packed */
  int i;
/*
 * Check arguments.
 */
  if(!types || !net) {
    lprintf(stderr, "net_to_obj: NULL %s argument.\n", !types ? "types":"net");
    return 1;
  };
/*
 * Look up the object definition.
 */
  info = net_obj_info(types, id);
  if(!info)
    return 1;

/*
 * A NULL object is legal if the object contains no members.
 */
  if(!obj && info->nmember != 0) {
    lprintf(stderr, "net_to_obj: NULL '%s' object.\n", info->name);
    return 1;
  };
/*
 * Un-pack the members of the network object sequentially.
 */
  for(i=0; i<info->nmember; i++) {
    const NetObjMember *member = info->member + i;
/*
 * Get a pointer to the start of the member in the object.
 */
    void *data = (char *)obj + member->offset;
/*
 * Extract the member according to its type.
 */
    switch(member->type) {
    case NET_ASCII:
      {
	long len;
	if(net_get_long(net, 1, (unsigned long int* )&len))
	  return 1;
	if(len >= member->ntype) {
	  lprintf(stderr,
		  "net_to_obj: String too long to fit into %s.%s[%d].\n",
		  info->name, member->name, member->ntype);
	  return 1;
	};
	if(net_get_char(net, len, (unsigned char* )data))
	  return 1;
	((char *) data)[len] = '\0';
      };
      break;
    case NET_BYTE:
      if(net_get_char(net, member->ntype, (unsigned char* )data))
	return 1;
      break;
    case NET_SHORT:
    case NET_ENUM:
    case NET_BOOL:
      if(net_get_short(net, member->ntype, (unsigned short* )data))
	return 1;
      break;
    case NET_MASK:
    case NET_LONG:
      if(net_get_long(net, member->ntype, (unsigned long* )data))
	return 1;
      break;
    case NET_FLOAT:
      if(net_get_float(net, member->ntype, (float* )data))
	return 1;
      break;
    case NET_DOUBLE:
      if(net_get_double(net, member->ntype, (double* )data))
	return 1;
      break;
    default:
      lprintf(stderr, "net_to_obj: Unknown data-type (%d).\n",
	      (int) member->type);
      return 1;
      break;
    };
  };
  return 0;
}

/*.......................................................................
 * Return the amount of buffer space needed to pack the largest of the
 * objects from the given table of object definitions.
 *
 * Input:
 *  types   NetObjTable *  The table of object definitions.
 * Output:
 *  return       size_t    The required network buffer space (bytes), or
 *                         0 on error.
 */
size_t net_max_obj_size(const NetObjTable *types)
{
  size_t max_size = 0;   /* The max object size */
  int i;
  for(i=0; i<types->nobj; i++) {
    size_t size = net_obj_size(types, i);
    if(size > max_size)
      max_size = size;
  };
  return max_size;
}

/*.......................................................................
 * Return the packed size of a given network object.
 *
 * Input:
 *  types    NetObjTable *  The table of object descriptions.
 *  id               int    The index of the object in the table.
 * Output:
 *  return        size_t    The packed size of the object. This will
 *                          be zero if the object contains no members,
 *                          or on error.
 */
size_t net_obj_size(const NetObjTable *types, int id)
{
  size_t size = 0;             /* The packed size of the object */
  const NetObjInfo *info;      /* The object definition */
  int i;
  
  // Lookup the object definition.

  info = net_obj_info(types, id);
  if(!info)
    return 0;
  
  // Add up the network sizes of the members of the object.

  for(i=0; i<info->nmember; i++) {
    const NetObjMember *member = info->member + i;
    size += net_type_size(member->type) * member->ntype;
    
    // Account for the extra length field that prefixes ASCII strings.

    if(member->type == NET_ASCII)
      size += net_type_size(NET_LONG);
  };
  return size;
}

/*.......................................................................
 * Return the packed size of a given network data-type, in bytes.
 *
 * Input:
 *  type     NetDataType   The type who's size is to be returned.
 * Output:
 *  return        size_t   The packed size of the data-type in bytes,
 *                         or 0 if the type is not known.
 */
size_t net_type_size(NetDataType type)
{
  switch(type) {
  case NET_ASCII:
  case NET_BYTE:
    return NET_CHAR_SIZE;
  case NET_SHORT:
  case NET_ENUM:
  case NET_BOOL:
    return NET_SHORT_SIZE;
  case NET_MASK:
  case NET_LONG:
    return NET_LONG_SIZE;
  case NET_FLOAT:
    return NET_FLOAT_SIZE;
  case NET_DOUBLE:
    return NET_DOUBLE_SIZE;
  };
  lprintf(stderr, "net_type_size: Unknown type (%d).\n", (int) type);
  return 0;
}

/*.......................................................................
 * Return the native size of a given network object on the local
 * architecture.
 *
 * Input:
 *  types    NetObjTable *  The table of object descriptions.
 *  id               int    The index of the object in the table.
 * Output:
 *  return        size_t    The native size of the object. This will
 *                          be zero if the object doesn't contain any
 *                          members (or if the object is unknown).
 */
size_t net_native_size(const NetObjTable *types, int id)
{
  const NetObjInfo *info;      /* The object definition */
/*
 * Lookup the object definition.
 */
  info = net_obj_info(types, id);
  if(!info)
    return 0;
  return info->native_size;
}

/*.......................................................................
 * Look up the definition of a given network object type.
 *
 * Input:
 *  types   NetObjTable *  A table of object definitions.
 *  id              int    The type of object, as an index into
 *                         types->objs[].
 * Output:
 *  return   NetObjInfo *  The definition of the object, or NULL if
 *                         types==NULL or id < 0 or id >= types->nobj.
 */
const NetObjInfo *net_obj_info(const NetObjTable *types, int id)
{
  if(!types) {
    lprintf(stderr, "net_obj_info: NULL 'types' argument.\n");
    return NULL;
  };
  if(id < 0 || id >= types->nobj) {
    lprintf(stderr, "net_obj_info: Object ID (%d) out of range 0..%d.\n",
	    id, types->nobj - 1);
    return NULL;
  };
  return types->info + id;
}
