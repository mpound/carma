#ifndef arraytemplate_h
#define arraytemplate_h

// This file defines the templates required by new_ArrayMap() for
// creating array maps. The resulting register map, and accompanying
// objects are defined in arraymap.h.

#include "carma/szaarrayutils/arraymap.h"
#include "carma/szaarrayutils/regtemplate.h"

// Uncomment the following for debugging information
// #define ARRAYMAP

/**.......................................................................
 * Collect the register map template information into a single object.
 */
typedef struct {
  char name[REG_NAME_LEN+1]; // An unambiguous name for the template
  RegTemplate* regtemplate;  // The template of registers
  char comment_[100];
} RegTemp;

/**.......................................................................
 * Collect the register map templates for an entire array into a
 * single object.
 */
typedef struct {
  RegTemp *templates;  // The array of register templates 
  unsigned ntemplate;  // The number of elements in templates[]
} ArrayTemplate;

/**.......................................................................
 * Create a map of available register maps.
 */
ArrayMap *new_ArrayMap(ArrayTemplate *arrayTmp);

/*.......................................................................
 * Retreive an array map template from a network buffer and convert
 * it to an array map.
 */
ArrayMap *net_get_ArrayMap(sza::array::NetBuf *net, bool old=false);

/**.......................................................................
 * Pack an Array Template into a network buffer
 */
int net_put_ArrayTemplate(ArrayTemplate *arrTmp, sza::array::NetBuf *net);

/*.......................................................................
 * Unpack an array map template from a network buffer.
 */
ArrayTemplate *net_get_ArrayTemplate(sza::array::NetBuf *net);

/*.......................................................................
 * Return the space needed to pack a given array-map template
 * into a NetBuf network buffer.
 */
long net_ArrayTemplate_size(ArrayTemplate *arrayTmp);

/**.......................................................................
 * The following destructor should only be applied the dynamically
 * allocated templates that are returned by net_get_ArrayTemplate().
 */
ArrayTemplate *del_ArrayTemplate(ArrayTemplate *arrayTmp);

#endif
