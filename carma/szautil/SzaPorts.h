#ifndef SZA_UTIL_SZAPORTS_H
#define SZA_UTIL_SZAPORTS_H

#include "carma/szautil/Directives.h"

#if DIR_IS_STABLE
#define SZA_PORT_OFFSET 5000
#else
#define SZA_PORT_OFFSET 6000
#endif

/*
 * The TCP/IP port on which the control program listens for connection
 * requests from the translator control process.
 */
#define CP_RTC_PORT 440+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the control-program listens for external
 * control clients.
 */
#define CP_CONTROL_PORT 441+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the control program listens for external
 * monitor clients.
 */
#define CP_MONITOR_PORT 442+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the control program listens for external
 * image monitor clients.
 */
#define CP_IM_MONITOR_PORT 443+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which that the control program listens for
 * connection requests from the translator scanner process.
 */
#define CP_RTS_PORT 444+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the control program listens for
 * connection requests from the translator optcam process.
 */
#define CP_RTO_PORT 445+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the translator listens for control
 * connections from antenna computers.
 */
#define TRANS_ANT_CONTROL_PORT 446+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the translator listens for scanner
 * connections from antenna computers.
 */
#define TRANS_ANT_SCANNER_PORT 447+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the translator listens for control
 * connections from the frame grabber
 */
#define TRANS_GRABBER_CONTROL_PORT 448+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the weather server listens for connection
 * requests
 */
#define TRANS_WEATHER_PORT 449+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the weather server listens for connection
 * requests
 */
#define TRANS_WX200_PORT 450+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the linelength server listens for connection
 * requests
 */
#define TRANS_LLCARMA_PORT 451+SZA_PORT_OFFSET

/*
 * The TCP/IP port on which the CARMA weather server listens for connection
 * requests
 */
#define TRANS_WXCARMA_PORT 452+SZA_PORT_OFFSET

#endif
