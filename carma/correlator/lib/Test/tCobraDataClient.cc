/* Read COBRA data from a server
 *
 * Use:
 * ----
 *
 * CobraDataClient -s <server> -p <port> -f <file>
 * 
 * <server>  remote data server   (default: local host)
 * <port>    specific port number (default: /etc/services lookup)
 * <file>    data dump file       (default: cobra_wideband_client_data.dat)
 * 
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netdb.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <cmath>
#include <vector>
#include <complex>
#include <iostream>
#include <iomanip>

#include "carma/correlator/lib/CobraDataFormat.h"
#include "carma/correlator/lib/CobraCorrelationInfo.h"
#include "carma/correlator/lib/CobraCorrelationData.h"
#include "carma/correlator/lib/CobraIntegrationData.h"
#include "carma/correlator/lib/CobraDataSim.h"

#include "carma/util/Program.h"
#include "carma/util/Time.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

carma::util::Time _time;

//
// @key server  ""          string  Server name
// @key port    @noDefault  int     Server port
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorbaDataClient
//

ssize_t readblock(int fd, char *buf, size_t size);

int Program::main()
{
	/* Service port number */
	char    service_name[] = "cobra.band1.data";
	struct  servent *service;
  	/* Client setup */
	int    sockfd;
    int    len;
    struct sockaddr_in address;
    struct in_addr inaddr;
    struct hostent *host = NULL;
	/* Misc */
	int   integ_cnt;
	int   status;
	//int   opt;

	const char kLocalhost[ ] = "localhost";

	/* Parameter extraction */
	const char * hostname = kLocalhost;
	int port = -1;
    
    const ::std::string sHostname = Program::getStringParameter( "server" );
    
    if ( sHostname.empty( ) )
        hostname = kLocalhost;
    else
        hostname = sHostname.c_str( );
    
	if ( Program::parameterWasSpecified( "port" ) ) {
        const int temp = Program::getIntParameter( "port" );
        
        if ( temp != 0 )
            port = temp;
	}
	
	/* Connect to server */
	if (hostname != NULL) {
   		/* If the hostname can be converted to an IP, do so. 
		 * If not, try to look it up in DNS. 
		 */
    	if (inet_pton(AF_INET, hostname, &inaddr)) {
    	    host = gethostbyaddr(
				(char *)&inaddr, 
				sizeof(inaddr), 
                AF_INET);
		} else {
    	    host = gethostbyname(hostname);
		}
    	if (host == NULL) {
			/* We can't find an IP number */
    	    cout << "Error: gethostbyname failed - "
				 << strerror(errno) 
				 << endl;
			return 1;
    	}
	}

	/* Create a socket for the client.  */
    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    if (sockfd < 0) {
        cout << "Error: Socket call failed - " 
			 << strerror(errno) 
		     << endl;
        return 1;
    }

	/* find the port number (look up the service in /etc/services) */
    if (port < 0) {
		service = getservbyname(service_name, "tcp");
	    if (!service) {
          cout << "Error: getservbyname failed! " << endl;
              // rh- status undefined here << strerror(status) << endl;
	        return 1;
	    }
		port = ntohs(service->s_port);
	}
	cout << "Contacting data server " 
		 << hostname << ":"
		 << port << endl;
	
	/* Name the socket, as agreed with the server.  */
    address.sin_family = AF_INET;
    if (hostname == NULL) {
		address.sin_addr.s_addr = inet_addr("127.0.0.1");
	} else {
		/* Take the first IP address associated with this hostname */
    	memcpy(&address.sin_addr, host->h_addr_list[0], 
				sizeof(address.sin_addr));
	}
	address.sin_port = htons(port);
    len = sizeof(address);

	/* Now connect our socket to the server's socket.  */
    status = connect(sockfd, (struct sockaddr *)&address, len);
    if(status < 0) {
        cout << "Error: Connection failed - " 
			 << strerror(errno) << endl;
        return 1;
    }

	cout << " - CONNECTED - reading data ... " << endl;
	
	integ_cnt = 1;
	/* Read until EOF */
	while (1) {
		cout << "-------------------------------------------\n"
			 << "Client: iteration " << integ_cnt
			 << endl;

		/* Reconstruct the objects */
		CobraDataFormat format;
		vector<CobraCorrelationInfo> info;
		CobraIntegrationData integ;

		/* Read the serialized data format */
		int format_size = format.size();
		vector<char> bytes(format_size + 1);
		bytes[0] = 0; /* big-endian data */
		len = readblock(sockfd, &bytes[1], format_size);
		if (len == 0) {
			cout << "EOF detected, exiting\n" << endl;
			break;
		}
		cout << "client: read " << len << " bytes" << endl;

		/* Reconstruct the data format */
		format.deserial(bytes);

		/* Extract parameters from the format object */
		int nauto          = format.getNauto();
		int nauto_samples  = format.getNautoSamples();
		int ncross         = format.getNcross();
		int ncross_samples = format.getNcrossSamples();

	  cout << "\nnauto          " << nauto
		   << "\nnauto_samples  " << nauto_samples
		   << "\nncross         " << ncross
		   << "\nncross_samples " << ncross_samples
		   << endl;
	
		/* Read the serialized correlation info */
		info.resize(nauto+ncross);
		int info_size = info[0].size();
		int infoarray_size = info_size*(nauto+ncross);
		bytes.resize(infoarray_size+1);
		len = readblock(sockfd, &bytes[1], infoarray_size);
		if (len == 0) {
			cout << "EOF detected, exiting\n" << endl;
			break;
		}
		cout << "client: read " << len << " bytes" << endl;

		/* Reconstruct the correlation info array */
		vector <char>::iterator vstart;
		vector <char>::iterator vend;
		for (int i = 0; i < (nauto+ncross); i++) {
			info[i].deserial(bytes);
			/* Delete info block from the raw data
			 * (but leave the endianness flag in there
			 */
			vstart = bytes.begin()+1;
			vend   = bytes.begin()+1+info_size;
			bytes.erase(vstart,vend);
		}
	
		/* The integration object needs a few parameters
		 * setup so it knows how much data to read.
		 */
		integ.setNauto(nauto);
		integ.setNautoSamples(nauto_samples);
		integ.setNcross(ncross);
		integ.setNcrossSamples(ncross_samples);

		/* Read the serialized correlation info */
		int integ_size = integ.size();
		bytes.resize(integ_size+1);
		len = readblock(sockfd, &bytes[1], integ_size);
		if (len == 0) {
			cout << "EOF detected, exiting\n" << endl;
			break;
		}
		cout << "client: read " << len << " bytes" << endl;

		/* Integration data */
		integ.deserial(bytes);

		/* Current time */
		double mjd = _time.MJD()*86400.0;
		cout.setf(ios::fixed);
		cout << "Current time  : " << setprecision(6) << mjd << endl;

		/* Timestamp */
		double timestamp = integ.getTimestamp();
		cout << "Data timestamp: " << setprecision(6) << timestamp << endl;
	
		/* Check */
		if (fabs(mjd - 0.5 - timestamp) > 0.1) {
			cout << "WARNING: timestamp appears to be wrong!" << endl;
		}
		
		/* TODO: translate the objects into Rick's objects */

		/* Check some data */
		for (int j = 0; j < nauto; j++) {
			CobraAutoSpectra a = integ.getAutoSpectra(j);
			vector<float> data = a.getData();
			cout << "Auto " << j << ": ";
			for (unsigned int i = 0; i < data.size(); i++) {
				cout << setprecision(1) << data.at(i) << " ";
			}
			cout << endl;
		}
		for (int k = 0; k < ncross; k++) {
			CobraCrossSpectra c = integ.getCrossSpectra(k);
			cout << "Cross " << k << ":\n";
			{	
				cout << "LSB ";
				vector< complex<float> > data = c.getData(0);
				for (unsigned int i = 0; i < data.size(); i++) {
					cout << setprecision(1) << abs(data.at(i)) << " ";
				}
				cout << endl;
			}
			{
				cout << "USB ";
				vector< complex<float> > data = c.getData(1);
				for (unsigned int i = 0; i < data.size(); i++) {
					cout << setprecision(1) << abs(data.at(i)) << " ";
				}
				cout << endl;
			}
		}
		
		integ_cnt++;
	}	
	close(sockfd);
    return 0;
}

ssize_t readblock(int fd, char *buf, size_t size)
{
	char *bufp;
	ssize_t bytesread;
	size_t bytestoread;
	size_t totalbytes;

	for (bufp = buf, bytestoread = size, totalbytes = 0;
		 bytestoread > 0;
		 bufp += bytesread, bytestoread -= bytesread) {
		bytesread = read(fd, bufp, bytestoread);
		if ((bytesread == 0) && (totalbytes == 0)) {
			return 0;
		}
		if (bytesread == 0) {
			errno = EINVAL;
			return -1;
		}
		if ((bytesread == -1) && (errno != EINTR)) {
			return -1;
		}
		if (bytesread == -1) {
			bytesread = 0;
		}
		totalbytes += bytesread;
	}
	return totalbytes;
}

