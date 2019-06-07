#include "carma/szautil/NetCmdForwarder.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetCmdForwarder::NetCmdForwarder() {}

/**.......................................................................
 * Destructor.
 */
NetCmdForwarder::~NetCmdForwarder() {}

/**
 * A virtual method to forward a command received from the ACC.
 * Make this virtual so that inheritors can completely redefine
 * what happens with a received command, if they wish.
 */
void forwardNetCmd(sza::util::NetCmd* netCmd) {};
