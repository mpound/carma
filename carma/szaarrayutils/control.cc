#include <string.h>

#include "carma/szaarrayutils/control.h"

#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/netobj.h"
#include "carma/szaarrayutils/lprintf.h"

#define ARRAY_DIM(array) (sizeof(array)/sizeof(array[0]))

namespace sza {
  namespace array{
    /*-----------------------------------------------------------------------
     * Separately describe each of the CcNetCmd objects in control.h.
     */
    static const NetObjMember input_cmd_members[] = {
      {"cmd", offsetof(CcInputCmd,cmd), NET_ASCII, CC_CMD_MAX+1}
    };
    
    /*
     * Collect all of the object definitions in an array.
     */
    static const NetObjInfo cmd_objects[] = {
      {"input",          input_cmd_members,         ARRAY_DIM(input_cmd_members),
       sizeof(CcInputCmd)},
    };
    
  }
}
/*
 * Form the global table of objects that is used by the functions
 * in netobj.h.
 */
const NetObjTable cc_cmd_table = {
  "cmd", sza::array::cmd_objects, sizeof(sza::array::cmd_objects)/sizeof(NetObjInfo), sizeof(sza::array::CcNetCmd)
};

namespace sza {
  namespace array {
    /*-----------------------------------------------------------------------
     * Separately describe each of the CcNetMsg objects in control.h.
     */
    static const NetObjMember log_msg_members[] = {
      {"seq",      offsetof(CcLogMsg,seq),      NET_LONG,   1},
      {"end",      offsetof(CcLogMsg,end),      NET_BOOL,   1},
      {"error",    offsetof(CcLogMsg,error),    NET_BOOL,   1},
      {"interactive", offsetof(CcLogMsg,interactive), NET_BOOL,   1},
      {"text",     offsetof(CcLogMsg,text),     NET_ASCII,  CC_MSG_MAX+1}
    };

    static const NetObjMember reply_msg_members[] = {
      {"seq",      offsetof(CcReplyMsg,seq),    NET_LONG,   1},
      {"end",      offsetof(CcReplyMsg,end),    NET_BOOL,   1},
      {"error",    offsetof(CcReplyMsg,error),  NET_BOOL,   1},
      {"text",     offsetof(CcReplyMsg,text),   NET_ASCII,  CC_MSG_MAX+1}
    };

    static const NetObjMember sched_msg_members[] = {
      {"text",     offsetof(CcSchedMsg,text),   NET_ASCII,  CC_MSG_MAX+1}
    };

    static const NetObjMember arc_msg_members[] = {
      {"text",     offsetof(CcArcMsg,text),     NET_ASCII,  CC_MSG_MAX+1}
    };

    static const NetObjMember page_msg_members[] = {
      {"text",     offsetof(CcPageMsg,text),    NET_ASCII,  CC_MSG_MAX+1},
      {"mask",     offsetof(CcPageMsg,mask),    NET_ENUM,   1},
      {"page",     offsetof(CcPageMsg,allow),   NET_BOOL,   1}
    };

    static const NetObjMember pageCond_msg_members[] = {
      {"mode",         offsetof(CcPageCondMsg, mode),         NET_LONG,   1},
      {"min",          offsetof(CcPageCondMsg, min),          NET_DOUBLE, 1},
      {"max",          offsetof(CcPageCondMsg, max),          NET_DOUBLE, 1},
      {"isDelta",      offsetof(CcPageCondMsg, isDelta),      NET_BOOL,   1},
      {"isOutOfRange", offsetof(CcPageCondMsg, isOutOfRange), NET_BOOL,   1},
      {"nFrame",       offsetof(CcPageCondMsg, nFrame),       NET_LONG,   1},
      {"text",         offsetof(CcPageCondMsg, text),         NET_ASCII,  CC_MSG_MAX+1},
    };
 
    static const NetObjMember config_msg_members[] = {
      {"mode",         offsetof(CcConfigMsg, mode),         NET_LONG,   1},
      {"array",        offsetof(CcConfigMsg, array),        NET_LONG,   1},
      {"config",       offsetof(CcConfigMsg, config),       NET_LONG,   1},
      {"iPad",         offsetof(CcConfigMsg, iPad),         NET_LONG,   1},
      {"iAnt",         offsetof(CcConfigMsg, iAnt),         NET_LONG,   1},
      {"antType",      offsetof(CcConfigMsg, antType),      NET_LONG,   1},
    };

    static const NetObjMember cmdTimeout_msg_members[] = {
      {"mode",         offsetof(CcCmdTimeoutMsg, mode),       NET_LONG,   1},
      {"seconds",      offsetof(CcCmdTimeoutMsg, seconds),    NET_LONG,   1},
      {"active",       offsetof(CcCmdTimeoutMsg, active),     NET_BOOL,   1},
    };

    static const NetObjMember ant_msg_members[] = {
      {"text",     offsetof(CcAntMsg,text),     NET_ASCII,  CC_MSG_MAX+1}
    };
    
    /*
     * Collect all of the message object definitions in an array.
     */
    static const NetObjInfo msg_objects[] = {
      {"log",          log_msg_members,         ARRAY_DIM(log_msg_members),
       sizeof(CcLogMsg)},
      {"reply",        reply_msg_members,       ARRAY_DIM(reply_msg_members),
       sizeof(CcReplyMsg)},
      {"sched",        sched_msg_members,       ARRAY_DIM(sched_msg_members),
       sizeof(CcSchedMsg)},
      {"arc",          arc_msg_members,         ARRAY_DIM(arc_msg_members),
       sizeof(CcArcMsg)},
      {"page",         page_msg_members,        ARRAY_DIM(page_msg_members),
       sizeof(CcPageMsg)},
      {"pageCond",     pageCond_msg_members,    ARRAY_DIM(pageCond_msg_members),
       sizeof(CcPageCondMsg)},
      {"config",       config_msg_members,      ARRAY_DIM(config_msg_members),
       sizeof(CcConfigMsg)},
      {"ant",          ant_msg_members,         ARRAY_DIM(ant_msg_members),
       sizeof(CcAntMsg)},
      {"cmdTimeout",   cmdTimeout_msg_members,  ARRAY_DIM(cmdTimeout_msg_members),
       sizeof(CcCmdTimeoutMsg)},
    };
  }
}
/*
 * Form the global table of message objects that is required by
 * the functions in netobj.h.
 */
const NetObjTable cc_msg_table = {
  "msg", sza::array::msg_objects, sizeof(sza::array::msg_objects)/sizeof(NetObjInfo), sizeof(sza::array::CcNetMsg)
};

