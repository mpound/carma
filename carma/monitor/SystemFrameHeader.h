#ifndef CARMA_MONITOR_SYSTEMFRAMEHEADER_H
#define CARMA_MONITOR_SYSTEMFRAMEHEADER_H

#include "carma/monitor/SystemFrame.h"


struct carma::monitor::SystemFrame::SystemHeader {
  unsigned short  numSubsystems;
  unsigned short  statusFlags;

  int             maxSubsystems;      // max number of subsystems

  int             maxMonitorPoints;   // max number of monitor points across
                                      // all maxSubsystems subsystems

  int             maxSamples;         // max number of monitor samples across
                                      // all maxSubsystems subsystems

  int             frameCount;         // timestamp for current system frame

  double          collatorWriteDelay; // offset from half-second at which frame
                                      // is supposed to be written to IPQ

  double          collatorWriteTime;  // time at which current frame was
                                      // written to the IPQ

  double          rawReadTime;        // time at which raw Carma frame was read
                                      // from the IPQ for fault processing

  double          finalWriteTime;     // time at which final frame is written
                                      // to the final IPQ (end product)

  int obsolete1;
  int obsolete2;
}; // 60 bytes (64 on x86_64)


#endif
