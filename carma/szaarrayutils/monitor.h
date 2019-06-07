#ifndef monitor_h
#define monitor_h

/*
 * This file defines attributes of the interface between the
 * control program and the monitor program.
 */
#include "carma/szautil/SzaPorts.h"

/*
 * Enumerate the types of messages that are passed from the
 * control-program to monitor clients.
 */
typedef enum {
  MC_SIZE_MSG,       /* Conveys the width of the communications channel */
  MC_REGMAP_MSG,     /* Conveys the current SZA register map */
  MC_REGS_MSG        /* Conveys values of the currently selected registers */
} CpToMonitor;

/*
 * Enumerate the types of messages that are passed from monitor clients
 * to the control program.
 */
typedef enum {
  MC_REGSET_MSG,     /* A new register selection set */
  MC_INTERVAL_MSG    /* A new sampling interval set */
} MonitorToCp;

/*
 * Enumerate the types of messages that are passed from the
 * control-program to image monitor clients.
 */
typedef enum {
  IMC_SIZE_MSG,     /* Conveys the width of the communications channel */
  IMC_IMAGE_MSG,    /* Conveys the current frame grabber image */
} CpToImMonitor;

#endif
