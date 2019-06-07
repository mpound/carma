#ifndef control_h
#define control_h

#include "carma/szaarrayutils/netobj.h"
#include "carma/szautil/SzaPorts.h"

namespace sza {
  namespace array {

    /*-----------------------------------------------------------------------
     * Define the ids of client -> control-program commands along with
     * the corresponding local command containers.
     */
    typedef enum {
      CC_INPUT_CMD              /* A command line typed by the user */
    } CcNetCmdId;
    
    /*
     * The CC_TEXT_LINE command conveys a single text command line to the
     * control program.
     */
    
    enum {CC_CMD_MAX=255}; /* The max size of a command string (excluding '\0') */
    
    typedef struct {
      char cmd[CC_CMD_MAX+1];  /* The ascii command string (including '\0') */
    } CcInputCmd;
    
    /*
     * Create a union of the above message containers.
     */
    typedef union {
      CcInputCmd input;        /* A command line to be compiled and executed */
    } CcNetCmd;
  }
}
/*
 * The following network-object description table, defined in control.c,
 * describes the above commands.
 */
extern const NetObjTable cc_cmd_table;

namespace sza {
  namespace array {

    /*-----------------------------------------------------------------------
     * Define the types of messages that are sent to control clients by the
     * control program.
     */
    typedef enum {
      CC_LOG_MSG,          /* A log message to be displayed */
      CC_REPLY_MSG,        /* A reply to a CC_INPUT_CMD command line */
      CC_SCHED_MSG,        /* A message regarding the state of the scheduler */
      CC_ARC_MSG,          /* A message regarding the state of the archiver */
      CC_PAGE_MSG,         /* A message regarding the pager status */
      CC_PAGECOND_MSG,     /* A message regarding the pager status */
      CC_CONFIG_MSG,       /* A message regarding the array configuration */
      CC_ANT_MSG,          /* A message regarding the state of the default 
			      antenna selection */
      CC_CMD_TIMEOUT_MSG
    } CcNetMsgId;
    
    /*
     * The following interface is for use in sending and receiving messages
     * sent from the control program to control clients.
     */
    //    enum {CC_MSG_MAX=131}; /* The max length of a message string (excluding '\0') */
    enum {CC_MSG_MAX=1000}; /* The max length of a message string (excluding '\0') */
    
    /*
     * Define a log message object.
     */
    typedef struct {
      unsigned seq;
      NetBool end;
      NetBool error;            /* True if the text is an error message */
      NetBool interactive;      /* True if the text was the result of an interactive message */
      char text[CC_MSG_MAX+1];  /* The ascii message string (including '\0') */
    } CcLogMsg;
    
    /*
     * Define a reply message object.
     */
    typedef struct {
      unsigned seq;
      NetBool end;
      NetBool error;            /* True if the text is an error message */
      char text[CC_MSG_MAX+1];  /* The ascii message string (including '\0') */
    } CcReplyMsg;
    
    /*
     * Define a message for reporting changes in the state of the
     * schedule queue.
     */
    typedef struct {
      char text[CC_MSG_MAX+1];  /* The status message */
    } CcSchedMsg;
    
    /*
     * Define a message for reporting changes in the state of the
     * archiver.
     */
    typedef struct {
      char text[CC_MSG_MAX+1];  /* The status message */
    } CcArcMsg;
    
    enum {
      PAGE_ENABLE = 0x1, // Set if this is an ordinary page enable/disable message
      PAGE_REG    = 0x2, // Set if this is page enable/disable
			 // message was sent because the pager was
			 // activated
      PAGE_MSG    = 0x4, // Set if this is page enable/disable
			 // message was sent because of some other reason
    };

    /*
     * Define a message for reporting changes in the state of the
     * pager
     */
    typedef struct {
      char text[CC_MSG_MAX+1];  /* The status message */
      NetEnum mask;
      NetBool allow;
    } CcPageMsg;
    
    /*
     * Define a message for reporting changes in the state of the
     * archiver.
     */
    typedef struct {
      unsigned mode;  
      double min;
      double max;
      bool isDelta;
      bool isOutOfRange;
      unsigned nFrame;
      char text[CC_MSG_MAX+1];  /* The status message */
    } CcPageCondMsg;

    enum PageCondMode {
      PAGECOND_ADD,
      PAGECOND_REMOVE,
      PAGECOND_CLEAR,
      PAGECOND_UPDATE,
    };

    /*
     * Define a message for reporting changes in the state of the
     * array configuration
     */
    typedef struct {
      unsigned mode;  
      unsigned array;
      unsigned config;
      int iPad;
      int iAnt;
      unsigned antType;
    } CcConfigMsg;

    enum ConfigMode {
      CONFIG_CONFIG,
      CONFIG_ADDANT,
      CONFIG_REMANT,
      CONFIG_CLEAR,
      CONFIG_UPDATE,
    };
    /*
     * Define a message for reporting changes in the state of the
     * timeout pager
     */
    typedef struct {
      unsigned mode;
      unsigned seconds;
      bool active;
    } CcCmdTimeoutMsg;

    enum CmdTimeoutMode {
      CT_ACTIVE,
      CT_TIMEOUT
    };

    /*
     * Define a message for reporting changes in the state of the
     * default antenna selection
     */
    typedef struct {
      char text[CC_MSG_MAX+1];  /* The status message */
    } CcAntMsg;

    /*
     * Define a union of all control-program -> control-client message types.
     */
    typedef union {
      CcLogMsg log;             /* A CC_LOG_MSG message */
      CcReplyMsg reply;         /* A CC_REPLY_MSG message */
      CcSchedMsg sched;         /* A CC_SCHED_MSG message */
      CcArcMsg arc;             /* A CC_ARC_MSG message */
      CcPageMsg page;           /* A CC_PAGE_MSG message */
      CcPageCondMsg pageCond;   /* A CC_PAGECOND_MSG message */
      CcConfigMsg config;       /* A CC_CONFIG_MSG message */
      CcAntMsg ant;             /* A CC_ANT_MSG message */
      CcCmdTimeoutMsg cmdTimeout;
    } CcNetMsg;
  }
}  
/*
 * The following network-object description table, defined in control.c,
 * describes the messages that are sent from the control program to
 * control clients.
 */
extern const NetObjTable cc_msg_table;

#endif
