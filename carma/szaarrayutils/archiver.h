#ifndef archiver_h
#define archiver_h

#ifndef szacontrol_h
typedef struct Archiver Archiver;
#endif

#include "carma/szautil/RegDescription.h"

Archiver *cp_Archiver(ControlProg *cp);

int get_reg_info(Archiver *arc, short iregmap, short board, short block, 
		 short index, unsigned long *val);

#endif
