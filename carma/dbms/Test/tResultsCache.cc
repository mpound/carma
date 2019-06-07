/**
 * @file
 * Implementation of the unit tests for the carma::dbms::ResultsCache
 *
 * @author: Dave Mehringer
 * @version $Id: tResultsCache.cc,v 1.7 2011/08/01 20:24:38 abeard Exp $
 *
 * $CarmaCopyright$
 *
 * @key conffile dbms/dbms.conf string config file for db connections
 *
 * @logger TEST_FACILITY carma.test.dbms.tResultsCache
 */

#include <unistd.h>
#include <vector>
#include "carma/dbms/ResultsCache.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/DBConfigurator.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/ref.hpp>
#include <boost/thread.hpp>

using namespace std;
using namespace carma::util;
using namespace carma::dbms;

class tBoostThread {
    
    public:

    tBoostThread(const string& name, const DBConnection * const dbc) 
        : dbc_( dbc ),
          finished_( false ) 
    {
        boost::thread newThread( boost::ref( *this ) );
        thread_.swap( newThread );
    }

    ~tBoostThread( )
    {
        // Try to join with the thread...
        thread_.interrupt();
        const boost::posix_time::time_duration joinTimeout =
            boost::posix_time::milliseconds( 500 );

        const bool joined = thread_.timed_join( joinTimeout );

        if ( !joined ) {
            ostringstream err;
            err << "tBoostThread d'tor - Unable to successfully "
                << "join with internal thread after " << joinTimeout << ". "
                << "Thread will be detached.";
            programLogErrorIfPossible( err.str() );
            thread_.detach();
        }
    }

    bool isFinished( ) {
        boost::mutex::scoped_lock scopelock( finishedMutex_ );
        return finished_;
    }
        
    void operator()() 
    try {
        //cout << "action for " << getName() << endl;
        ResultsCache &rc = ResultsCache::getCache(dbc_);
        rc.getFullMonitorConfigurationTable();
        boost::mutex::scoped_lock scopelock( finishedMutex_ );
        finished_ = true;
    } catch (...) {
        boost::mutex::scoped_lock scopelock( finishedMutex_ );
        finished_ = true;
        throw;
    }


    private:
    const DBConnection *dbc_;
    boost::thread thread_;
    bool finished_;
    boost::mutex finishedMutex_;

};



int Program::main() {
    try {
        string conffile = getConfFile(getStringParameter("conffile"));
        DBConfigurator *dbconf = new DBConfigurator(conffile);
        DBConnection *dbc = DBConnectionFactory::createConnection(dbconf);
        delete dbconf;
        tBoostThread ct1("tResultsCacheThread 1",dbc);
        tBoostThread ct2("tResultsCacheThread 2",dbc);
        vector<tBoostThread *> threads;
        threads.push_back(&ct1);
        threads.push_back(&ct2);
        for(unsigned int i=0; i < threads.size(); i++) {
            //cout << i << " is running " << threads[i]->isRunning() << endl;
            //cout << i << " is running " << threads[i]->isRunning() << endl;
            //sleep(2);
        }
        int j=0;
        for(unsigned int i=0; i < threads.size(); i++) {
            while(!threads[i]->isFinished()) {
                usleep(10000);
                j++;
            }
            cout << i << " is finished" << endl;
        }
        cout << "exiting normally after " << j << " sleeps" << endl;
    } catch ( const std::exception & e ) {
        cerr << "std::exception caught in main:\n"  << "    " 
             << e.what( ) << endl;
        return EXIT_FAILURE;
    } catch ( ... ) {
        cerr << "unknown type of exception caught in main" << endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}

