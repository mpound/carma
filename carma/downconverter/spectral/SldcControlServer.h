/** @file
 * carma::downconverter::SldcControlServer class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.5 $
 * $Id: SldcControlServer.h,v 1.5 2012/01/25 22:26:09 abeard Exp $
 */
#ifndef CARMA_DOWNCONVERTER_SLDCCONTROLSERVER_H
#define CARMA_DOWNCONVERTER_SLDCCONTROLSERVER_H

// No inclusions

namespace carma {

namespace corba {
    class Server;
} // namespace corba

namespace monitor {
    class SldcSubsystem;
} // namespace monitor

namespace downconverter {

    class SldcControlServerPimpl;
    class SldcMaster;

    /**
     * Spectral line downconverter control server class.
     */
    class SldcControlServer {
    public:

        /**
         * Constructor
         */
        explicit SldcControlServer( carma::corba::Server & server,
                                    carma::downconverter::SldcMaster & master,
                                    carma::monitor::SldcSubsystem & sldcMon );
        
        /**
         * Destructor
         */
        virtual ~SldcControlServer();

        /**
         * Run the control server.
         * This method blocks until the server is shutdown via a remote request.
         */ 
        void run();

    private:

        // Prevent duplication and assignment
        SldcControlServer(const SldcControlServer &);
        SldcControlServer &operator=(const SldcControlServer &);

        SldcControlServerPimpl * pimpl_; // Private implementation
        carma::corba::Server & server_;

    }; // End class SldcControlServer
}} // End namespace carma::downconverter
#endif
