/** @file
 * Declaration for carma::util::NTPQuery class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2005/03/05 00:56:08 $
 * $Id: NTPQuery.h,v 1.1 2005/03/05 00:56:08 abeard Exp $
 *
 * Copyright Notice for ntp modified code
 * "Clone me," says Dolly sheepishly
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_UTIL_NTPQ_H
#define CARMA_UTIL_NTPQ_H

namespace carma {
namespace util {

    /**
     * NTPQuery class.
     * This class is responsible for carrying out ntp queries.  It 
     * essentially wraps a good deal of the ntpq c-code found in the ntpq
     * portion of the ntp package.
     */
    class NTPQuery {
    public:
   
        /**
         * Retrieve NTP delay in ms.
         * The delay is the estimated roundtrip network delay between the peer
         * and the current host in milliseconds.  It is calculated as follows:
         * <p>delay = (T4 - T1) - (T3 - T2).</p>
         * where T1 is the timestamp of a client request message, T2 is the
         * servers timestamp upon arrival, T3 is the server response message
         * timestamp and T4 is the clients timestamp upon receipt of the
         * response.  On a typical LAN, this number should be in the 
         * sub ten ms range.
         */
        double getDelay();
        
        /**
         * Retrieve NTP offset in ms.
         * The offset is the NTP estimate of how much the current host's clock
         * differs from a peers.  It is calculated as follows:
         * <p>offset = [(T2 - T1) + (T3 - T4)] / 2</p>
         * where T1 is the timestamp of a client request message, T2 is the
         * servers timestamp upon arrival, T3 is the server response message
         * timestamp and T4 is the clients timestamp upon receipt of the
         * response.  On a typical LAN, this number should be in the
         * sub ms range.
         */
        double getOffset();

    protected:

    private:

    }; // End class NTPQuery
} // End namespace util
} // End namespace carma
#endif
