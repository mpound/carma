#ifndef szascript_h
#define szascript_h

#include "carma/szaarrayutils/script.h"

Script *new_SzaScript(ControlProg *cp, int batch, HashTable *signals);
Script *del_SzaScript(Script *cs);

/*
 * An object of the following type is allocated by each schedule script
 * to record non-script objects that need to be garbage collected when 
 * the script is deleted or discarded. It can be found in Script::data.
 */
typedef struct {
  int ref_count;    /* The reference count of the parent Script */
  List *schedules;  /* The list of schedules in use by the script */
} ScheduleData;

#endif
