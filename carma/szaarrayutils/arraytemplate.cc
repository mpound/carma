#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "carma/szaarrayutils/arraymaprev.h"
#include "carma/szaarrayutils/arraytemplate.h"
#include "carma/szaarrayutils/lprintf.h"

using namespace sza::array;

/*.......................................................................
 * Pack a register map template into a network buffer.
 *
 * Input:
 *  arraytmp  ArrayTemplate *  The register map template to be packed.
 * Input/Output:
 *  net          NetBuf *  The network buffer in which to pack the
 *                         template. Note that it is the caller's
 *                         responsibility to call net_start_put() and
 *                         net_end_put().
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
int net_put_ArrayTemplate(ArrayTemplate *arraytmp, NetBuf *net)
{
  int regtemplate;   // The index of the register template being packed

  // Check arguments.

  if(!arraytmp || !net) {
    lprintf(stderr, "net_put_ArrayTemplate: NULL %s argument.\n",
	    !arraytmp ? "arraytmp" : "net");
    return 1;
  };

  // Record the array-map object revision count.

  unsigned long arraymap_revision = ARRAYMAP_REVISION;
  if(net_put_long(net, 1, &arraymap_revision))
    return 1;

  // Record the count of the number of templates.

  unsigned short ntemplate = arraytmp->ntemplate;
  if(net_put_short(net, 1, &ntemplate))
    return 1;
  
  // Pack each register template

  for(regtemplate=0; regtemplate < (int)arraytmp->ntemplate; regtemplate++) {
    RegTemp* regtemp = arraytmp->templates + regtemplate;

    unsigned short name_len = strlen(regtemp->name);
    
    // Record the name of the template

    if(net_put_short(net, 1, &name_len) ||
       net_put_char(net, name_len, (unsigned char* )regtemp->name))
      return 1;

    // And pack the template 

    if(net_put_RegTemplate(regtemp->regtemplate, net))
      return 1;
  };

  return 0;
}

/*.......................................................................
 * Unpack an array map template from a network buffer.
 *
 * Input:
 *  net          NetBuf *  The network buffer from which to unpack the
 *                         template. Note that it is the callers
 *                         responsibility to call net_start_get() and
 *                         net_end_get().
 * Output:
 *  return  ArrayTemplate *  The unpacked array map template, or NULL
 *                         on error. This can be deleted via a call to
 *                         del_ArrayTemplate().
 */
ArrayTemplate *net_get_ArrayTemplate(NetBuf *net)
{
  ArrayTemplate *arraytmp=0; // The new template 
  unsigned regtemplate;    // The index of the register template being processed
  
  // Check arguments.

  if(!net) {
    lprintf(stderr, "net_get_ArrayTemplate: NULL net argument.\n");
    return NULL;
  };
  
  // Allocate the container of the template.

  arraytmp = (ArrayTemplate *) malloc(sizeof(ArrayTemplate));

  if(!arraytmp) {
    lprintf(stderr, "net_get_ArrayTemplate: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_ArrayTemplate().

  arraytmp->templates = NULL;
  arraytmp->ntemplate = 0;
  
  // Unpack the array-map object revision count and see if the
  // array template can be decoded by this function.

  unsigned long arraymap_revision;
  if(net_get_long(net, 1, &arraymap_revision))
    return del_ArrayTemplate(arraytmp);

  if(arraymap_revision > ARRAYMAP_REVISION) {
    lprintf(stderr, "net_get_ArrayTemplate: Incompatible register map.\n");
    return del_ArrayTemplate(arraytmp);
  };
  
  // Unpack the count of the number of templates.

  unsigned short ntemplate;

  if(net_get_short(net, 1, &ntemplate))
    return del_ArrayTemplate(arraytmp);
  
  arraytmp->ntemplate = ntemplate;
  
  if(ntemplate < 1) {
    lprintf(stderr, "net_get_ArrayTemplate: ntemplate <= 0.\n");
    return del_ArrayTemplate(arraytmp);
  };

  // Allocate the array of templates

  arraytmp->templates = (RegTemp* ) malloc(sizeof(RegTemp)*
					       arraytmp->ntemplate);

  if(!arraytmp->templates) {
    lprintf(stderr, "net_get_ArrayTemplate: Insufficient memory.\n");
    return del_ArrayTemplate(arraytmp);
  };
  
  // Initialize the array.

  for(regtemplate=0; regtemplate < arraytmp->ntemplate; regtemplate++) {
    RegTemp* regtmp = arraytmp->templates + regtemplate;
    regtmp->regtemplate = NULL;
  }

  // Un-pack each template

  for(regtemplate=0; regtemplate < arraytmp->ntemplate; regtemplate++) {
    RegTemp* regtemp = arraytmp->templates + regtemplate;

    unsigned short name_len;

    // Un-pack the name of the template.

    if(net_get_short(net, 1, &name_len))
      return del_ArrayTemplate(arraytmp);

    if(name_len > REG_NAME_LEN) {
      lprintf(stderr, "net_get_ArrayTemplate: Template name too long.\n");
      return del_ArrayTemplate(arraytmp);
    }

    if(net_get_char(net, name_len, (unsigned char* )regtemp->name))
      return del_ArrayTemplate(arraytmp);

    regtemp->name[name_len] = '\0';

    // Now read the template itself

    if((regtemp->regtemplate=net_get_RegTemplate(net, arraymap_revision))==NULL)
      return del_ArrayTemplate(arraytmp);
  }

  return arraytmp;
}

/*.......................................................................
 * Return the space needed to pack a given array-map template
 * into a NetBuf network buffer.
 *
 * Input:
 *  arraytmp   ArrayTemplate *  The set to be characterized.
 * Output:
 *  return     long    The number of bytes needed to pack a full
 *                     set of register ranges.
 */
long net_ArrayTemplate_size(ArrayTemplate *arraytmp)
{
  unsigned regtemplate; // The index of the register template being processed
  long size = 0;        // The byte-count to be returned 
  
  // Reserve space for the count of the number of register templates

  size += NET_SHORT_SIZE;
  
  // Reserve space for the details of each register template.

  for(regtemplate=0; regtemplate < arraytmp->ntemplate; regtemplate++) {
    RegTemp* regtemp = arraytmp->templates + regtemplate;

    // Reserve space for the name of each template

    size += NET_SHORT_SIZE +                  // name_len 
      NET_CHAR_SIZE * strlen(regtemp->name);  // name[] 

    // And for the template itself.

    size += net_RegTemplate_size(regtemp->regtemplate);
  };

  return size;
}

/*.......................................................................
 * Delete an array template returned previously by net_get_ArrayTemplate().
 *
 * Input:
 *  arraytmp ArrayTemplate *   The array template to be deleted.
 * Output:
 *  return   ArrayTemplate *   The deleted template (ie. NULL).
 */
ArrayTemplate *del_ArrayTemplate(ArrayTemplate *arraytmp)
{
  unsigned int i;
  if(arraytmp) {
    if(arraytmp->templates != NULL) {
      for(i=0; i < arraytmp->ntemplate; i++) {
	RegTemp* regtmp = arraytmp->templates + i;
	regtmp->regtemplate = del_RegTemplate(regtmp->regtemplate);
      };
      free(arraytmp->templates);
    };
    free(arraytmp);
  };
  return NULL;
}

/*.......................................................................
 * Retreive an array map template from a network buffer and convert
 * it to a register map.
 *
 * Input:
 *  net    NetBuf *  The network buffer from which to unpack the register
 *                   map. It is left to the caller to call
 *                   net_start_get() and net_end_get().
 * Output:
 *  return ArrayMap *  The array map, or NULL on error.
 */
ArrayMap *net_get_ArrayMap(NetBuf *net, bool old)
{
  ArrayTemplate* at=0;  // The template of the register map 
  ArrayMap* arraymap=0; // The corresponding array map 
  
  // Retrieve the register template from the network buffer.

  at = net_get_ArrayTemplate(net);

  if(!at)
    return NULL;
  
  // Construct the register map from the template.

#if 1

  // CARMA version -- per-board regs should NOT be added

  arraymap = new ArrayMap(at, old, false);

#else

  // SZA version -- assume that per-board regs should be added

  arraymap = new ArrayMap(at, old, true);

#endif
  
  // Discard the redundant template.

  at = del_ArrayTemplate(at);
  
  // Return the array map (this will be NULL if new_ArrayMap()
  // failed).

  return arraymap;
}
