#ifndef scanner_h
#define scanner_h

#include "carma/szaarrayutils/rtcnetcoms.h"
#include "carma/szaarrayutils/szaregs.h"

/*
 * Scanner information that is shared between sun control program and
 * VxWorks tasks.
 */
#include "carma/szautil/SzaPorts.h"

/*
 * Enumerate control-program -> scanner-task message types.
 */
typedef enum {
  SCAN_GREETING     /* Sent by a client on connecting to the scanner task */
} CpToScanner;

/*
 * Enumerate scanner-task -> control-program message types.
 */
typedef enum {
  SCAN_DATA_MSG   /* A message containing register values */
} ScannerToCp;

/*
 * Determine the max number of register snapshots to be buffered when
 * the scanner capture rate exceeds the network bandwidth.
 */
enum {SCAN_MAX_FRAME=100};

/*
 * Set the size of the scanner network read-buffer.
 */
enum {SCAN_MAX_CMD_SIZE=100};

// Deprecated -- calculations assuming every register is an unsigned
// int type

/*
 * The network buffer that is used to communicate register frames
 * to the control program has size:
 *    sizeof(unsigned) * (SCAN_HEADER_DIM + regmap->narchive)
 */
#define SCAN_HEADER_DIM ((NET_PREFIX_LEN+sizeof(unsigned)-1)/sizeof(unsigned))
#define SCAN_HEADER_PAD (SCAN_HEADER_DIM * sizeof(unsigned) - NET_PREFIX_LEN)
#define SCAN_BUFF_SIZE(nreg) (sizeof(unsigned) * (SCAN_HEADER_DIM + (nreg)))

// Calculations for register maps which can consist of variable-size
// types

/*
 * The network buffer that is used to communicate register frames
 * to the control program has size:
 *
 *    sizeof(unsigned) * (SCAN_HEADER_DIM + regmap->narchive)
 */

#define SCAN_BUFF_BYTE_SIZE(nByte) (sizeof(unsigned) * SCAN_HEADER_DIM + nByte)

/*
 * Set limits on the allowable integration intervals. Intervals
 * are specified as power of 2 multipliers of the basic
 * (12.8us x 2^SCAN_BASE_HARDWARE_INTERVAL)
 * second sampling time.
 */
enum {
  SCAN_BASE_HARDWARE_INTERVAL = 5, /* Hardware basis interval */
  SCAN_MIN_HARDWARE_INTERVAL = 5,  /* Prevent high-frequency interrupts */
  SCAN_MAX_HARDWARE_INTERVAL = 12, /* Prevent hardware overflow */
  SCAN_DEF_HARDWARE_INTERVAL = 11  /* The default interval [0.84 seconds] */
};

/*-----------------------------------------------------------------------
 * VxWorks-specific definitions.
 */
#ifdef VXW

TASK_NEW_FN(new_Scanner);
TASK_DEL_FN(del_Scanner);
TASK_INI_FN(ini_Scanner);
TASK_TST_FN(tst_Scanner);
TASK_END_FN(end_Scanner);

int sza_scanner_task(void *resources);

 /* The max number of queued messages on the scanner message queue */

#define SCAN_SMQ_LEN 100

 /* Timeout for sends to the scanner message queue - clock ticks */

#define SCAN_SMQ_TIMEOUT 60

 /* Name the IP network interface for use in ifAddrGet() and ifAddrSet() */

#define SCAN_IP_INTERFACE "ei0"

/*
 * Set the name of the I/O system binary semaphore that is used by
 * the system timing generator to trigger frame acquisition.
 */
#define SCAN_STROBE_DEVICE "/sem/strobe"

/* Forward a network command to the scanner task */

TASK_FWD_FN(forward_scanner_netcmd);
/*
 * Network commands forwarded from the control program to this task by
 * the controller task, are passed in a container of the following type.
 */
typedef struct {
  NetCmdId id;           /* The type of command contained in 'cmd' */
  RtcNetCmd cmd;         /* The body of the command */
} ScanNetCmd;

/*
 * A structure of the following type is used by all tasks that
 * send messages to the scanner task over its message queue. It
 * contains the type and contents of the message.
 */
typedef struct {
/*
 * Enumerate the message type.
 */
  enum {               /* The type of message */
    SCAN_SHUTDOWN,     /* Shutdown the scanner task */
    SCAN_HEARTBEAT,    /* Report task status to controller */
    SCAN_GRABBER,      /* Scan the frame grabber for a new image. */
    SCAN_SETREG_DONE,  /* A setreg command is done -- forwarded by the
			  probe task */
    SCAN_TV_OFFSET_DONE,/* A tv_offset command is done -- forwarded by
                          the tracker task */
    SCAN_NET_CMD       /* A forwarded network command */
  } type;
/*
 * Describe the contents of each message type.
 */
  union {                 /* A union of all message-type message-contents */
    ScanNetCmd net;       /* SCAN_NET_CMD */
    unsigned setreg_seq;  /* SCAN_SETREG_DONE */
    unsigned tv_offset_seq;/* SCAN_TV_OFFSET_DONE */
  } body;
} ScannerMessage;

/* 
 * Function used to send the scanner task messages via its message
 * queue.
 */
int scanner_send_message(ScannerMessage *msg, size_t size);

/*-----------------------------------------------------------------------
 * Host-specific definitions.
 */
#else

#endif

#endif
