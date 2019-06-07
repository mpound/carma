#ifndef PMACFAKE_H
#define PMACFAKE_H

/**
 * @file PmacFake.h
 * 
 * Tagged: Thu Nov 13 16:53:45 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <sys/select.h> // select()
#include <sys/types.h>  // recv()
#include <sys/socket.h>

#include "carma/szaarrayutils/tcpip.h"

#include "carma/antenna/sza/antenna/control/PmacCommand.h"
#include "carma/antenna/sza/antenna/control/PmacComms.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      class PmacFake {
      public:
	
	inline void connect()
	  {
	    fd_ = udp_server_sock(PMAC_HOST_PORT, 1);
	  }
	
	inline void serviceMsgQ()
	  {
	    fd_set read_fds;
	    
	    FD_ZERO(&read_fds);
	    FD_SET(fd_, &read_fds);
	    
	    while(select(fd_+1, &read_fds, NULL, NULL, NULL) > 0) {
	      processEthCmd();
	    }
	  }
	
	inline void processEthCmd() 
	  {
	    recv(fd_, (void*) &cmd_, sizeof(PmacCommand::EthCmd)+PMAC_DATA_MAX_LEN, 0);
	    cout << "Got a message: ";
	    fprintf(stdout, "%x\n", cmd_.info_.request_);
	  }
	
      private:
	
	int fd_;
	
	PmacCommand::EthCmd cmd_;
	
      }; // End class PmacFake
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
